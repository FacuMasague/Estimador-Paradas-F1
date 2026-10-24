"""Actualizar datos de OpenF1 en SQLite para analizarlos posteriormente desde R.

Python descarga y conserva datos de la fuente. La selección de carreras, el
tratamiento de lluvia, la limpieza estadística y el modelo pertenecen a R.
Ejecutar --help para ver descargas parciales, reanudación y consulta de estado.
"""

import argparse
from contextlib import closing
from datetime import datetime, timedelta, timezone
from email.utils import parsedate_to_datetime
import hashlib
import json
from pathlib import Path
import sqlite3
import ssl
import sys
import time
from urllib.error import HTTPError, URLError
from urllib.parse import urlencode
from urllib.request import Request, urlopen

try:
    import certifi
except ImportError:
    certifi = None  # Se conserva la verificación TLS con los certificados del sistema.

API_URL = "https://api.openf1.org/v1/"
RUTA_BASE = Path(__file__).resolve().with_name("datosOpenF1.sqlite3")
VERSION_BASE = 2
INTERVALO_CONSULTAS = 2.1  # Menos de 30 solicitudes por minuto.

# Campos de la fuente -> columnas SQLite. Los tiempos de duración están en
# segundos; las edades de neumáticos, en vueltas. No se calculan otras variables.
# Las listas de mini-sectores se guardan como JSON; todos los escalares quedan
# accesibles como columnas normales para DBI/RSQLite.
CONJUNTOS = {
    "drivers": ("pilotos", {
        "driver_number": "INTEGER", "full_name": "TEXT", "first_name": "TEXT",
        "last_name": "TEXT", "name_acronym": "TEXT", "broadcast_name": "TEXT",
        "team_name": "TEXT", "team_colour": "TEXT", "headshot_url": "TEXT",
        "country_code": "TEXT",
    }),
    "weather": ("clima", {
        "date": "TEXT", "air_temperature": "REAL", "track_temperature": "REAL",
        "rainfall": "INTEGER", "humidity": "REAL", "pressure": "REAL",
        "wind_direction": "REAL", "wind_speed": "REAL",
    }),
    "laps": ("vueltas", {
        "driver_number": "INTEGER", "lap_number": "INTEGER", "date_start": "TEXT",
        "lap_duration": "REAL", "is_pit_out_lap": "INTEGER",
        "duration_sector_1": "REAL", "duration_sector_2": "REAL", "duration_sector_3": "REAL",
        "i1_speed": "REAL", "i2_speed": "REAL", "st_speed": "REAL",
        "segments_sector_1": "TEXT", "segments_sector_2": "TEXT", "segments_sector_3": "TEXT",
    }),
    "stints": ("stints", {
        "driver_number": "INTEGER", "stint_number": "INTEGER", "compound": "TEXT",
        "lap_start": "INTEGER", "lap_end": "INTEGER", "tyre_age_at_start": "INTEGER",
    }),
    "pit": ("boxes", {
        "driver_number": "INTEGER", "date": "TEXT", "lap_number": "INTEGER",
        "lane_duration": "REAL", "stop_duration": "REAL", "pit_duration": "REAL",
    }),
    "race_control": ("control_carrera", {
        "date": "TEXT", "category": "TEXT", "flag": "TEXT", "message": "TEXT",
        "scope": "TEXT", "sector": "INTEGER", "driver_number": "INTEGER",
        "lap_number": "INTEGER", "qualifying_phase": "INTEGER",
    }),
    "position": ("posiciones", {
        "date": "TEXT", "driver_number": "INTEGER", "position": "INTEGER",
    }),
    "intervals": ("intervalos", {
        "date": "TEXT", "driver_number": "INTEGER", "gap_to_leader": "TEXT", "interval": "TEXT",
    }),
}
ENDPOINTS_INICIALES = ("drivers", "weather", "laps", "stints", "pit", "race_control")

SQL_GP = """CREATE TABLE {tabla} (
    meeting_key INTEGER PRIMARY KEY, temporada INTEGER NOT NULL, nombre TEXT NOT NULL,
    fecha_inicio TEXT, fecha_fin TEXT, actualizado_en TEXT NOT NULL,
    cancelado INTEGER, circuito_key INTEGER, circuito TEXT, pais TEXT,
    presente_en_fuente INTEGER NOT NULL DEFAULT 1,
    respuesta_id INTEGER REFERENCES respuestas_api(id)
)"""
SQL_SESIONES = """CREATE TABLE {tabla} (
    session_key INTEGER PRIMARY KEY,
    meeting_key INTEGER NOT NULL REFERENCES grandes_premios(meeting_key),
    nombre TEXT NOT NULL, tipo TEXT, fecha_inicio TEXT, fecha_fin TEXT,
    actualizado_en TEXT NOT NULL, cancelada INTEGER,
    presente_en_fuente INTEGER NOT NULL DEFAULT 1,
    respuesta_id INTEGER REFERENCES respuestas_api(id)
)"""


class ErrorActualizacion(Exception):
    """Error de descarga o validación que se muestra sin ocultar datos ausentes."""


class RespuestaAPI(list):
    """Registros más el sobre original, incluido el 404 explícito sin resultados."""

    def __init__(self, registros, codigoHTTP=200, original=None):
        super().__init__(registros)
        self.codigoHTTP = codigoHTTP
        self.original = registros if original is None else original


def ahoraUTC():
    return datetime.now(timezone.utc)


def fechaUTC(texto):
    """Convertir una fecha con zona horaria; devolver None si falta o es inválida."""
    if not isinstance(texto, str):
        return None
    try:
        valor = datetime.fromisoformat(texto.replace("Z", "+00:00"))
        return valor.astimezone(timezone.utc) if valor.tzinfo is not None else None
    except ValueError:
        return None


def textoFecha(texto):
    valor = fechaUTC(texto)
    return valor.isoformat() if valor is not None else None


def serializar(valor):
    return json.dumps(valor, ensure_ascii=False, sort_keys=True, allow_nan=False)


def identificador(valor, nombre):
    if isinstance(valor, bool) or not isinstance(valor, int) or valor <= 0:
        raise ErrorActualizacion(f"Identificador inválido o ausente: {nombre}={valor!r}")
    return valor


def abrirBase(ruta=RUTA_BASE):
    """Abrir SQLite y activar relaciones y espera ante lectores concurrentes."""
    conexion = sqlite3.connect(ruta, timeout=30)
    conexion.row_factory = sqlite3.Row
    conexion.execute("PRAGMA foreign_keys = ON")
    conexion.execute("PRAGMA busy_timeout = 30000")
    return conexion


def _copiarColumnas(conexion, origen, destino):
    antiguas = {fila[1] for fila in conexion.execute(f'PRAGMA table_info("{origen}")')}
    nuevas = [fila[1] for fila in conexion.execute(f'PRAGMA table_info("{destino}")')]
    comunes = [nombre for nombre in nuevas if nombre in antiguas]
    columnas = ", ".join(f'"{nombre}"' for nombre in comunes)
    conexion.execute(f'INSERT INTO "{destino}" ({columnas}) SELECT {columnas} FROM "{origen}"')


def crearTablas(conexion):
    """Crear o migrar la estructura conservando las reuniones y prácticas previas.

    La versión anterior exigía fechas NOT NULL; el catálogo puede contener
    cancelaciones o fechas desconocidas. La migración conserva esos casos como
    NULL y agrega metadatos. Nunca fabrica una fecha para poder insertar una fila.
    """
    version = conexion.execute("PRAGMA user_version").fetchone()[0]
    if version > VERSION_BASE:
        raise ErrorActualizacion("La base usa una versión más nueva que este programa.")
    existentes = {fila[0] for fila in conexion.execute("SELECT name FROM sqlite_master WHERE type='table'")}
    if version == 0 and "grandes_premios" in existentes:
        conexion.commit()
        conexion.execute("PRAGMA foreign_keys = OFF")
        try:
            with conexion:
                conexion.execute("BEGIN IMMEDIATE")
                conexion.execute(SQL_GP.format(tabla="grandes_premios_nueva"))
                conexion.execute(SQL_SESIONES.format(tabla="sesiones_nueva"))
                _copiarColumnas(conexion, "grandes_premios", "grandes_premios_nueva")
                if "sesiones" in existentes:
                    _copiarColumnas(conexion, "sesiones", "sesiones_nueva")
                    conexion.execute("DROP TABLE sesiones")
                conexion.execute("DROP TABLE grandes_premios")
                conexion.execute("ALTER TABLE grandes_premios_nueva RENAME TO grandes_premios")
                conexion.execute("ALTER TABLE sesiones_nueva RENAME TO sesiones")
                if conexion.execute("PRAGMA foreign_key_check").fetchall():
                    raise ErrorActualizacion("La base anterior contiene referencias inválidas; migración cancelada.")
        finally:
            conexion.execute("PRAGMA foreign_keys = ON")
    with conexion:
        conexion.execute("BEGIN IMMEDIATE")
        conexion.execute("""CREATE TABLE IF NOT EXISTS respuestas_api (
            id INTEGER PRIMARY KEY, endpoint TEXT NOT NULL, parametros_json TEXT NOT NULL,
            obtenido_en TEXT NOT NULL, sha256 TEXT NOT NULL, contenido_json TEXT NOT NULL,
            codigo_http INTEGER NOT NULL DEFAULT 200,
            UNIQUE(endpoint, parametros_json, sha256)
        )""")
        camposRespuesta = {fila[1] for fila in conexion.execute("PRAGMA table_info(respuestas_api)")}
        if "codigo_http" not in camposRespuesta:
            conexion.execute("ALTER TABLE respuestas_api ADD COLUMN codigo_http INTEGER NOT NULL DEFAULT 200")
        conexion.execute("""CREATE TABLE IF NOT EXISTS descargas (
            clave TEXT PRIMARY KEY, endpoint TEXT NOT NULL, parametros_json TEXT NOT NULL,
            estado TEXT NOT NULL CHECK(estado IN ('ok','vacia','error')),
            intento_en TEXT NOT NULL, exito_en TEXT, proxima_revision TEXT,
            filas INTEGER, error TEXT, firma_sesion TEXT,
            respuesta_id INTEGER REFERENCES respuestas_api(id)
        )""")
        conexion.execute("""CREATE TABLE IF NOT EXISTS control_actualizador (
            clave TEXT PRIMARY KEY, valor REAL NOT NULL
        )""")
        if "grandes_premios" not in existentes:
            conexion.execute(SQL_GP.format(tabla="grandes_premios"))
        if "sesiones" not in existentes and not (version == 0 and "grandes_premios" in existentes):
            conexion.execute(SQL_SESIONES.format(tabla="sesiones"))
        for tabla, campos in CONJUNTOS.values():
            columnas = ", ".join(f'"{nombre}" {tipo}' for nombre, tipo in campos.items())
            conexion.execute(f'''CREATE TABLE IF NOT EXISTS "{tabla}" (
                session_key INTEGER NOT NULL REFERENCES sesiones(session_key),
                registro INTEGER NOT NULL, meeting_key INTEGER,
                {columnas}, actualizado_en TEXT NOT NULL,
                respuesta_id INTEGER NOT NULL REFERENCES respuestas_api(id),
                PRIMARY KEY (session_key, registro)
            )''')
        conexion.execute("CREATE INDEX IF NOT EXISTS idx_sesiones_reunion ON sesiones(meeting_key)")
        conexion.execute("CREATE INDEX IF NOT EXISTS idx_sesiones_fecha ON sesiones(fecha_fin)")
        for tabla in ("pilotos", "vueltas", "stints", "boxes", "control_carrera"):
            conexion.execute(f'CREATE INDEX IF NOT EXISTS "idx_{tabla}_piloto" ON "{tabla}"(session_key,driver_number)')
        conexion.execute(f"PRAGMA user_version = {VERSION_BASE}")
    if conexion.execute("PRAGMA foreign_key_check").fetchall():
        raise ErrorActualizacion("Hay referencias inválidas entre tablas; revisar la base antes de descargar.")


def respaldarAntesDeMigrar(conexion, ruta):
    """Respaldar automáticamente la base del tutorial antes de cambiar su estructura."""
    version = conexion.execute("PRAGMA user_version").fetchone()[0]
    existe = conexion.execute("SELECT 1 FROM sqlite_master WHERE name='grandes_premios'").fetchone()
    if version == 0 and existe:
        destino = Path(ruta).with_name(f"{Path(ruta).stem}_antes_migracion_{ahoraUTC():%Y%m%dT%H%M%S%f}.sqlite3")
        with closing(sqlite3.connect(destino)) as copia:
            conexion.backup(copia)
        print(f"Respaldo previo a la migración: {destino}", flush=True)


class ClienteOpenF1:
    """Acceso público con TLS, reintentos y límite compartido por la base local."""

    def __init__(self, conexion):
        self.conexion = conexion
        self.consultas = 0
        self.contextoSSL = ssl.create_default_context(cafile=certifi.where()) if certifi else ssl.create_default_context()

    def esperarTurno(self):
        # Reservar un turno de forma atómica. También espacia dos procesos que
        # utilicen esta misma base, sin mantener una transacción durante la red.
        self.conexion.execute("BEGIN IMMEDIATE")
        try:
            fila = self.conexion.execute("SELECT valor FROM control_actualizador WHERE clave='proximo_turno'").fetchone()
            turno = max(time.time(), fila[0] if fila else 0)
            self.conexion.execute("INSERT OR REPLACE INTO control_actualizador VALUES ('proximo_turno', ?)",
                                  (turno + INTERVALO_CONSULTAS,))
            self.conexion.commit()
        except Exception:
            self.conexion.rollback()
            raise
        time.sleep(max(0, turno - time.time()))

    def obtener(self, endpoint, parametros):
        url = API_URL + endpoint + "?" + urlencode(sorted(parametros.items()))
        for intento in range(3):
            self.esperarTurno()
            self.consultas += 1
            try:
                solicitud = Request(url, headers={"User-Agent": "EstimadorF1-Universidad/1.0", "Accept": "application/json"})
                with urlopen(solicitud, context=self.contextoSSL, timeout=30) as respuesta:
                    datos = json.load(respuesta)
                if not isinstance(datos, list) or any(not isinstance(fila, dict) for fila in datos):
                    raise ErrorActualizacion(f"Formato inesperado en {endpoint}: se esperaba una lista de registros.")
                return RespuestaAPI(datos)
            except HTTPError as error:
                if error.code == 404:
                    try:
                        detalle = json.loads(error.read(4096))
                    except (ValueError, TypeError, UnicodeError):
                        detalle = None
                    finally:
                        error.close()
                    # Comportamiento comprobado en la API pública. No convertir
                    # cualquier 404 en vacío ni perder el cuerpo/código originales.
                    if detalle == {"detail": "No results found."}:
                        return RespuestaAPI([], 404, detalle)
                else:
                    error.close()
                if error.code not in (429, 500, 502, 503, 504) or intento == 2:
                    raise ErrorActualizacion(f"HTTP {error.code} al consultar {endpoint} {parametros}") from error
                retry = error.headers.get("Retry-After") if error.headers else None
                try:
                    espera = float(retry)
                except (ValueError, TypeError):
                    try:
                        espera = (parsedate_to_datetime(retry) - ahoraUTC()).total_seconds()
                    except (ValueError, TypeError, OverflowError):
                        espera = 5 * (intento + 1)
                # Respetar el tiempo completo sin hacer solicitudes durante la espera.
                restante = max(2.1, espera)
                print(f"  HTTP {error.code}; reintento en {restante:.0f} s...", flush=True)
                while restante > 0:
                    tramo = min(restante, 30)
                    time.sleep(tramo)
                    restante -= tramo
            except (URLError, TimeoutError, OSError, ValueError) as error:
                if intento == 2:
                    raise ErrorActualizacion(f"No se pudo leer {endpoint}: {error}") from error
                time.sleep(3 * (intento + 1))
        raise ErrorActualizacion(f"No se pudo consultar {endpoint}")


def _clave(endpoint, parametros):
    return endpoint + "?" + urlencode(sorted(parametros.items()))


def _firmaSesion(sesion):
    return serializar({k: sesion[k] for k in ("fecha_inicio", "fecha_fin", "tipo", "cancelada")})


def necesitaDescarga(conexion, endpoint, parametros, ahora, forzar=False, firma=None):
    if forzar:
        return True
    estado = conexion.execute("SELECT * FROM descargas WHERE clave=?", (_clave(endpoint, parametros),)).fetchone()
    if estado is None or estado["firma_sesion"] != firma:
        return True
    proxima = fechaUTC(estado["proxima_revision"])
    return proxima is not None and proxima <= ahora


def _guardarRespuesta(conexion, endpoint, parametros, datos, instante):
    contenido = serializar(getattr(datos, "original", datos))
    parametrosJSON = serializar(parametros)
    huella = hashlib.sha256(contenido.encode("utf-8")).hexdigest()
    conexion.execute("""INSERT INTO respuestas_api
        (endpoint,parametros_json,obtenido_en,sha256,contenido_json,codigo_http) VALUES (?,?,?,?,?,?)
        ON CONFLICT(endpoint,parametros_json,sha256) DO NOTHING""",
        (endpoint, parametrosJSON, instante, huella, contenido, getattr(datos,"codigoHTTP",200)))
    return conexion.execute("SELECT id FROM respuestas_api WHERE endpoint=? AND parametros_json=? AND sha256=?",
                            (endpoint, parametrosJSON, huella)).fetchone()[0]


def _guardarEstado(conexion, endpoint, parametros, estado, ahora, proxima, filas=None,
                   error=None, firma=None, respuestaID=None):
    # Un error conserva la referencia a la última respuesta válida y sus datos.
    conexion.execute("""INSERT INTO descargas
        (clave,endpoint,parametros_json,estado,intento_en,exito_en,proxima_revision,
         filas,error,firma_sesion,respuesta_id) VALUES (?,?,?,?,?,?,?,?,?,?,?)
        ON CONFLICT(clave) DO UPDATE SET estado=excluded.estado,
          intento_en=excluded.intento_en, proxima_revision=excluded.proxima_revision,
          error=excluded.error, firma_sesion=excluded.firma_sesion,
          exito_en=COALESCE(excluded.exito_en,descargas.exito_en),
          filas=COALESCE(excluded.filas,descargas.filas),
          respuesta_id=COALESCE(excluded.respuesta_id,descargas.respuesta_id)""",
        (_clave(endpoint,parametros), endpoint, serializar(parametros), estado,
         ahora.isoformat(), ahora.isoformat() if estado != "error" else None,
         proxima.isoformat() if proxima else None, filas, error, firma, respuestaID))


def guardarGranPremio(conexion, registro, respuestaID, instante):
    clave = identificador(registro.get("meeting_key"), "meeting_key")
    identificador(registro.get("year"), "year")
    if not isinstance(registro.get("meeting_name"), str):
        raise ErrorActualizacion("Reunión sin nombre; se conserva la versión local anterior.")
    conexion.execute("""INSERT INTO grandes_premios
        (meeting_key,temporada,nombre,fecha_inicio,fecha_fin,actualizado_en,cancelado,
         circuito_key,circuito,pais,presente_en_fuente,respuesta_id)
        VALUES (?,?,?,?,?,?,?,?,?,?,1,?) ON CONFLICT(meeting_key) DO UPDATE SET
        temporada=excluded.temporada,nombre=excluded.nombre,fecha_inicio=excluded.fecha_inicio,
        fecha_fin=excluded.fecha_fin,actualizado_en=excluded.actualizado_en,cancelado=excluded.cancelado,
        circuito_key=excluded.circuito_key,circuito=excluded.circuito,pais=excluded.pais,
        presente_en_fuente=1,respuesta_id=excluded.respuesta_id""",
        (clave,registro["year"],registro["meeting_name"],textoFecha(registro.get("date_start")),
         textoFecha(registro.get("date_end")),instante,registro.get("is_cancelled"),
         registro.get("circuit_key"),registro.get("circuit_short_name"),registro.get("country_name"),respuestaID))


def guardarSesion(conexion, registro, respuestaID, instante):
    clave = identificador(registro.get("session_key"), "session_key")
    reunion = identificador(registro.get("meeting_key"), "meeting_key")
    if not isinstance(registro.get("session_name"), str):
        raise ErrorActualizacion("Sesión sin nombre; se conserva la versión local anterior.")
    conexion.execute("""INSERT INTO sesiones
        (session_key,meeting_key,nombre,tipo,fecha_inicio,fecha_fin,actualizado_en,
         cancelada,presente_en_fuente,respuesta_id) VALUES (?,?,?,?,?,?,?,?,1,?)
        ON CONFLICT(session_key) DO UPDATE SET meeting_key=excluded.meeting_key,
        nombre=excluded.nombre,tipo=excluded.tipo,fecha_inicio=excluded.fecha_inicio,
        fecha_fin=excluded.fecha_fin,actualizado_en=excluded.actualizado_en,
        cancelada=excluded.cancelada,presente_en_fuente=1,respuesta_id=excluded.respuesta_id""",
        (clave,reunion,registro["session_name"],registro.get("session_type"),
         textoFecha(registro.get("date_start")),textoFecha(registro.get("date_end")),
         instante,registro.get("is_cancelled"),respuestaID))


def guardarCatalogo(conexion, endpoint, temporada, datos, respuestaID, instante):
    # Un catálogo vacío queda identificado como tal y no borra filas previas.
    if not datos:
        return
    for registro in datos:
        if registro.get("year") != temporada:
            raise ErrorActualizacion(f"{endpoint}: registro de otra temporada.")
    if endpoint == "meetings":
        conexion.execute("UPDATE grandes_premios SET presente_en_fuente=0 WHERE temporada=?", (temporada,))
        for registro in datos:
            guardarGranPremio(conexion, registro, respuestaID, instante)
    else:
        conexion.execute("""UPDATE sesiones SET presente_en_fuente=0 WHERE meeting_key IN
            (SELECT meeting_key FROM grandes_premios WHERE temporada=?)""", (temporada,))
        for registro in datos:
            guardarSesion(conexion, registro, respuestaID, instante)


def guardarConjunto(conexion, endpoint, sesion, datos, respuestaID, instante):
    """Reemplazar atómicamente la foto actual, conservando originales y duplicados.

    registro es la posición (desde 1) dentro de la respuesta, NO un número de
    vuelta ni identificador deportivo. Permite conservar duplicados de la fuente
    para que R decida su tratamiento. No se impone una relación con pilotos:
    la fuente puede tener vueltas aun cuando falten sus metadatos de piloto.
    """
    tabla, campos = CONJUNTOS[endpoint]
    clave = sesion["session_key"]
    filas = []
    for posicion, registro in enumerate(datos, 1):
        if registro.get("session_key", clave) != clave:
            raise ErrorActualizacion(f"{endpoint}: la respuesta contiene datos de otra sesión.")
        if registro.get("meeting_key", sesion["meeting_key"]) != sesion["meeting_key"]:
            raise ErrorActualizacion(f"{endpoint}: la respuesta contiene datos de otra reunión.")
        valores = []
        for campo in campos:
            valor = registro.get(campo)
            if isinstance(valor, (list, dict)) or (endpoint == "intervals" and valor is not None):
                valor = serializar(valor) if isinstance(valor, (list, dict)) else str(valor)
            valores.append(valor)
        filas.append((clave,posicion,registro.get("meeting_key"),*valores,instante,respuestaID))
    columnas = ["session_key", "registro", "meeting_key", *campos, "actualizado_en", "respuesta_id"]
    columnasSQL = ",".join(f'"{columna}"' for columna in columnas)
    marcadores = ",".join("?" for _ in columnas)
    conexion.execute(f'DELETE FROM "{tabla}" WHERE session_key=?', (clave,))
    conexion.executemany(f'INSERT INTO "{tabla}" ({columnasSQL}) VALUES ({marcadores})', filas)


def descargarYGuardar(cliente, endpoint, parametros, ahora, sesion=None):
    """Confirmar datos, original y estado juntos; los errores no vacían tablas."""
    conexion = cliente.conexion
    firma = _firmaSesion(sesion) if sesion is not None else None
    try:
        datos = cliente.obtener(endpoint, parametros)
        obtenido = ahoraUTC()
        instante = obtenido.isoformat()
        with conexion:
            respuestaID = _guardarRespuesta(conexion, endpoint, parametros, datos, instante)
            if sesion is None:
                guardarCatalogo(conexion, endpoint, parametros["year"], datos, respuestaID, instante)
            else:
                guardarConjunto(conexion, endpoint, sesion, datos, respuestaID, instante)
            # Vacío, error y pendiente son situaciones diferentes. Un vacío no
            # demuestra que una práctica fuera seca o que un piloto no participó.
            if not datos:
                proxima = obtenido + timedelta(hours=24)
            elif sesion is None:
                proxima = obtenido if parametros["year"] == ahora.year else None
            elif (obtenido - fechaUTC(sesion["fecha_fin"])) < timedelta(days=3):
                proxima = obtenido + timedelta(hours=6)
            else:
                proxima = None
            _guardarEstado(conexion,endpoint,parametros,"ok" if datos else "vacia",obtenido,
                           proxima,len(datos),firma=firma,respuestaID=respuestaID)
        return len(datos)
    except (ErrorActualizacion, sqlite3.Error, ValueError, TypeError) as error:
        with conexion:
            _guardarEstado(conexion,endpoint,parametros,"error",ahoraUTC(),
                           ahoraUTC()+timedelta(minutes=15),error=str(error),firma=firma)
        raise ErrorActualizacion(str(error)) from error


def actualizarBase(cliente, temporada, ahora, revisarCatalogo=False, forzar=False):
    """Actualizar el catálogo sin aplicar filtros que pertenecen al análisis en R."""
    fallos = 0
    for endpoint in ("meetings", "sessions"):
        parametros = {"year": temporada}
        if necesitaDescarga(cliente.conexion,endpoint,parametros,ahora,
                            forzar or revisarCatalogo or temporada == ahora.year):
            try:
                cantidad = descargarYGuardar(cliente,endpoint,parametros,ahora)
                print(f"  {temporada} {endpoint}: {cantidad} registros", flush=True)
            except ErrorActualizacion as error:
                fallos += 1
                print(f"  ERROR {temporada} {endpoint}: {error}", flush=True)
                if endpoint == "meetings":
                    break  # No insertar sesiones sin sus reuniones.
    return fallos


def seleccionarSesiones(conexion, desde, hasta, tipos, ahora, esperaMinutos=60, claves=None):
    """Disponibilidad técnica: solo sesiones terminadas; sin criterios deportivos."""
    filas = conexion.execute("""SELECT s.* FROM sesiones s JOIN grandes_premios gp
        ON gp.meeting_key=s.meeting_key WHERE gp.temporada BETWEEN ? AND ?
        AND s.presente_en_fuente=1 AND gp.presente_en_fuente=1 AND gp.cancelado=0
        ORDER BY s.fecha_inicio,s.session_key""", (desde,hasta)).fetchall()
    elegidas = []
    for fila in filas:
        if claves and fila["session_key"] not in claves:
            continue
        if fila["tipo"] not in tipos:
            continue
        if fila["cancelada"] != 0:
            continue  # Desconocida no equivale a False.
        fin, inicio = fechaUTC(fila["fecha_fin"]), fechaUTC(fila["fecha_inicio"])
        if inicio is not None and fin is not None and inicio < fin <= ahora-timedelta(minutes=esperaMinutos):
            elegidas.append(dict(fila))
    return elegidas


def mostrarEstado(conexion):
    existentes = {fila[0] for fila in conexion.execute("SELECT name FROM sqlite_master WHERE type='table'")}
    print("\nFilas guardadas:")
    for tabla in ("grandes_premios","sesiones",*(x[0] for x in CONJUNTOS.values())):
        if tabla in existentes:
            print(f"  {tabla}: {conexion.execute(f'SELECT COUNT(*) FROM {tabla}').fetchone()[0]}")
    if "descargas" in existentes:
        print("Estados de las consultas realizadas:")
        for fila in conexion.execute("SELECT estado,COUNT(*) FROM descargas GROUP BY estado"):
            print(f"  {fila[0]}: {fila[1]}")
        pendientes = conexion.execute("SELECT endpoint,parametros_json,error FROM descargas WHERE estado='error' LIMIT 5").fetchall()
        for fila in pendientes:
            print(f"  Error pendiente: {fila[0]} {fila[1]} — {fila[2]}")
        print("Las consultas aún no realizadas no tienen una fila en descargas.")


def argumentos(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--base",type=Path,default=RUTA_BASE,help="Archivo SQLite (se conserva el nombre del proyecto)")
    parser.add_argument("--desde",type=int,default=2023,help="Primera temporada")
    parser.add_argument("--hasta",type=int,default=ahoraUTC().year,help="Última temporada")
    parser.add_argument("--solo-catalogo",action="store_true",help="Actualizar reuniones y sesiones sin descargar sus datos")
    parser.add_argument("--estado",action="store_true",help="Leer el estado local sin red ni cambios de estructura")
    parser.add_argument("--sesion",type=int,action="append",help="Limitar los datos a un session_key; se puede repetir")
    parser.add_argument("--tipos",nargs="+",default=["Practice","Race"],help="Tipos de sesión a descargar; por defecto Practice Race")
    parser.add_argument("--endpoints",nargs="+",choices=tuple(CONJUNTOS),default=list(ENDPOINTS_INICIALES))
    parser.add_argument("--max-sesiones",type=int,help="Procesar como máximo N sesiones pendientes por ejecución")
    parser.add_argument("--espera-minutos",type=int,default=60,help="Margen tras finalizar la sesión para evitar acceso en vivo")
    parser.add_argument("--revisar-catalogo",action="store_true",help="Reconsultar también catálogos de temporadas anteriores")
    parser.add_argument("--forzar",action="store_true",help="Reconsultar todos los conjuntos seleccionados, incluidos los ya guardados")
    args = parser.parse_args(argv)
    args.endpoints = list(dict.fromkeys(args.endpoints))
    if not 2023 <= args.desde <= args.hasta <= ahoraUTC().year:
        parser.error("Se requiere 2023 <= desde <= hasta <= año actual")
    if args.max_sesiones is not None and args.max_sesiones < 1:
        parser.error("max-sesiones debe ser positivo")
    if args.espera_minutos < 0 or any(clave <= 0 for clave in args.sesion or []):
        parser.error("espera-minutos debe ser >=0 y los identificadores deben ser positivos")
    return args


def main(argv=None):
    args = argumentos(argv)
    conexion = None
    try:
        if args.estado:
            if not args.base.is_file():
                raise ErrorActualizacion(f"No existe la base: {args.base}")
            conexion = sqlite3.connect(args.base.resolve().as_uri()+"?mode=ro",uri=True)
            mostrarEstado(conexion)
            return 0
        conexion = abrirBase(args.base)
        respaldarAntesDeMigrar(conexion,args.base)
        crearTablas(conexion)
        cliente = ClienteOpenF1(conexion)
        ahora = ahoraUTC()
        print(f"Base local: {args.base.resolve()}",flush=True)
        print("OpenF1 histórico gratuito. Python solo descarga y guarda; el análisis se realiza en R.",flush=True)
        fallos = 0
        for temporada in range(args.desde,args.hasta+1):
            fallos += actualizarBase(cliente,temporada,ahora,args.revisar_catalogo,args.forzar)
        procesadas = 0
        if not args.solo_catalogo:
            sesiones = seleccionarSesiones(conexion,args.desde,args.hasta,args.tipos,ahora,args.espera_minutos,set(args.sesion or []))
            encontradas = {s["session_key"] for s in sesiones}
            if args.sesion and set(args.sesion)-encontradas:
                print("Sesiones solicitadas no disponibles para descargar:",sorted(set(args.sesion)-encontradas),flush=True)
                fallos += 1
            for sesion in sesiones:
                endpoints = [e for e in args.endpoints if e != "intervals" or sesion["tipo"] == "Race"]
                pendientes = [e for e in endpoints if necesitaDescarga(conexion,e,{"session_key":sesion["session_key"]},ahora,
                                                                      args.forzar,_firmaSesion(sesion))]
                if not pendientes:
                    continue
                if args.max_sesiones is not None and procesadas >= args.max_sesiones:
                    print("Límite de sesiones alcanzado; la próxima ejecución retomará lo pendiente.",flush=True)
                    break
                procesadas += 1
                print(f"\nSesión {sesion['session_key']} — {sesion['nombre']} ({procesadas})",flush=True)
                for endpoint in pendientes:
                    try:
                        cantidad = descargarYGuardar(cliente,endpoint,{"session_key":sesion["session_key"]},ahora,sesion)
                        print(f"  {endpoint}: {cantidad} registros"+(" (respuesta vacía)" if not cantidad else ""),flush=True)
                    except ErrorActualizacion as error:
                        fallos += 1
                        print(f"  ERROR {error}",flush=True)
        print(f"\nSolicitudes remotas realizadas: {cliente.consultas}. Sesiones procesadas: {procesadas}.")
        mostrarEstado(conexion)
        erroresGuardados = conexion.execute("SELECT COUNT(*) FROM descargas WHERE estado='error'").fetchone()[0]
        return 1 if fallos or erroresGuardados else 0
    except KeyboardInterrupt:
        print("\nInterrumpido. Se conservan las descargas confirmadas; podés volver a ejecutar.")
        return 130
    except (ErrorActualizacion,sqlite3.Error,OSError) as error:
        print(f"Error: {error}",file=sys.stderr)
        return 1
    finally:
        if conexion is not None:
            conexion.close()


if __name__ == "__main__":
    raise SystemExit(main())
