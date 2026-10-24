"""Pruebas del actualizador. Todos los registros de este archivo son sintéticos.

Ejecutar: python3 -m unittest -v test_catalogo
No requiere red ni modifica la base de datos del proyecto.
"""

from contextlib import closing, redirect_stdout
from datetime import datetime, timedelta, timezone
from io import BytesIO, StringIO
import json
from pathlib import Path
import sqlite3
import tempfile
import unittest
from unittest.mock import MagicMock, patch
from urllib.error import HTTPError

import estimadorF1 as p

AHORA = datetime(2026, 10, 24, 12, tzinfo=timezone.utc)
GP = {"meeting_key": 100, "year": 2023, "meeting_name": "GP SINTÉTICO",
      "date_start": "2023-03-01T10:00:00+00:00", "date_end": "2023-03-03T16:00:00+00:00",
      "is_cancelled": False}
SESION = {"session_key": 200, "meeting_key": 100, "year": 2023,
          "session_name": "Practice 1", "session_type": "Practice",
          "date_start": "2023-03-01T10:00:00+00:00", "date_end": "2023-03-01T11:00:00+00:00",
          "is_cancelled": False}


class ClienteFalso:
    def __init__(self, conexion, datos):
        self.conexion = conexion
        self.datos = datos
        self.consultas = 0

    def obtener(self, endpoint, parametros):
        self.consultas += 1
        dato = self.datos[endpoint]
        if isinstance(dato, Exception):
            raise dato
        return dato


class BasePrueba(unittest.TestCase):
    def setUp(self):
        self.db = p.abrirBase(":memory:")
        p.crearTablas(self.db)
        cliente = ClienteFalso(self.db, {"meetings": [GP], "sessions": [SESION]})
        with patch.object(p, "ahoraUTC", return_value=AHORA), redirect_stdout(StringIO()):
            p.actualizarBase(cliente, 2023, AHORA)
        self.sesion = dict(self.db.execute("SELECT * FROM sesiones WHERE session_key=200").fetchone())

    def tearDown(self):
        self.db.close()

    def descargar(self, endpoint, datos, sesion=None):
        cliente = ClienteFalso(self.db, {endpoint: datos})
        with patch.object(p, "ahoraUTC", return_value=AHORA):
            return p.descargarYGuardar(cliente, endpoint, {"session_key": 200}, AHORA,
                                       self.sesion if sesion is None else sesion)

    def test_catalogo_repetido_no_duplica_ni_reconsulta_anios_consolidados(self):
        cliente = ClienteFalso(self.db, {"meetings": [GP], "sessions": [SESION]})
        with redirect_stdout(StringIO()):
            self.assertEqual(p.actualizarBase(cliente, 2023, AHORA), 0)
        self.assertEqual(cliente.consultas, 0)
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM grandes_premios").fetchone()[0], 1)
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM sesiones").fetchone()[0], 1)

    def test_catalogo_actual_se_revisa_en_cada_ejecucion(self):
        nuevoGP = {**GP, "year": AHORA.year, "meeting_key": 101}
        nuevaSesion = {**SESION, "year": AHORA.year, "session_key": 201, "meeting_key": 101}
        cliente = ClienteFalso(self.db, {"meetings": [nuevoGP], "sessions": [nuevaSesion]})
        with patch.object(p, "ahoraUTC", return_value=AHORA), redirect_stdout(StringIO()):
            p.actualizarBase(cliente, AHORA.year, AHORA)
            p.actualizarBase(cliente, AHORA.year, AHORA)
        self.assertEqual(cliente.consultas, 4)
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM sesiones").fetchone()[0], 2)

    def test_sesiones_nuevas_del_mismo_gp_se_agregan_sin_exigir_tres_practicas(self):
        nueva = {**SESION, "session_key": 201, "session_name": "Practice 2"}
        cliente = ClienteFalso(self.db, {"meetings": [GP], "sessions": [SESION, nueva]})
        with patch.object(p, "ahoraUTC", return_value=AHORA), redirect_stdout(StringIO()):
            p.actualizarBase(cliente, 2023, AHORA, revisarCatalogo=True)
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM sesiones").fetchone()[0], 2)

    def test_futuro_y_cancelacion_se_guardan_como_catalogo_pero_no_se_descargan(self):
        futuro = {**SESION, "session_key": 201, "date_start": (AHORA+timedelta(days=1)).isoformat(),
                  "date_end": (AHORA+timedelta(days=1,hours=1)).isoformat()}
        cancelada = {**SESION, "session_key": 202, "is_cancelled": True}
        desconocida = {**SESION, "session_key": 203, "is_cancelled": None}
        cliente = ClienteFalso(self.db, {"sessions": [SESION, futuro, cancelada, desconocida]})
        with patch.object(p, "ahoraUTC", return_value=AHORA):
            p.descargarYGuardar(cliente, "sessions", {"year": 2023}, AHORA)
        seleccion = p.seleccionarSesiones(self.db, 2023, 2023, ["Practice"], AHORA)
        self.assertEqual([s["session_key"] for s in seleccion], [200])
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM sesiones").fetchone()[0], 4)

    def test_gp_cancelado_no_descarga_sus_sesiones(self):
        with self.db:
            self.db.execute("UPDATE grandes_premios SET cancelado=1")
        self.assertEqual(p.seleccionarSesiones(self.db, 2023, 2023, ["Practice"], AHORA), [])

    def test_fecha_ausente_y_reciente_no_equivalen_a_datos_historicos_disponibles(self):
        with self.db:
            self.db.execute("UPDATE sesiones SET fecha_fin=NULL")
        self.assertFalse(p.seleccionarSesiones(self.db,2023,2023,["Practice"],AHORA))
        with self.db:
            self.db.execute("UPDATE sesiones SET fecha_inicio=?,fecha_fin=?",
                            ((AHORA-timedelta(hours=2)).isoformat(),(AHORA-timedelta(minutes=15)).isoformat()))
        self.assertFalse(p.seleccionarSesiones(self.db,2023,2023,["Practice"],AHORA))
        self.assertEqual(len(p.seleccionarSesiones(self.db,2023,2023,["Practice"],AHORA,esperaMinutos=0)),1)

    def test_formato_sprint_no_es_un_filtro_de_analisis_en_python(self):
        with self.db:
            self.db.execute("UPDATE sesiones SET tipo='Sprint',nombre='Sprint'")
        self.assertEqual(len(p.seleccionarSesiones(self.db,2023,2023,["Sprint"],AHORA)),1)

    def test_clima_guarda_nulos_y_no_interpreta_la_lluvia(self):
        self.descargar("weather", [{"session_key":200,"meeting_key":100,"date":"2023-03-01T10:00:00Z",
                                     "rainfall":None,"track_temperature":31.5}])
        fila=self.db.execute("SELECT * FROM clima").fetchone()
        self.assertIsNone(fila["rainfall"])
        self.assertIsNone(fila["air_temperature"])
        self.assertEqual(fila["track_temperature"],31.5)

    def test_duplicados_y_campos_nuevos_de_fuente_se_conservan(self):
        vuelta={"session_key":200,"meeting_key":100,"driver_number":7,"lap_number":2,
                "lap_duration":None,"segments_sector_1":[2049,2048],"campo_futuro":"original"}
        self.descargar("laps", [vuelta,vuelta])
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM vueltas").fetchone()[0],2)
        fila=self.db.execute("SELECT * FROM vueltas ORDER BY registro").fetchone()
        self.assertIsNone(fila["lap_duration"])
        self.assertEqual(json.loads(fila["segments_sector_1"]),[2049,2048])
        respuesta=json.loads(self.db.execute("SELECT contenido_json FROM respuestas_api WHERE id=?",(fila["respuesta_id"],)).fetchone()[0])
        self.assertEqual(respuesta,[vuelta,vuelta])

    def test_error_no_vacia_datos_ni_su_ultima_respuesta_valida(self):
        self.descargar("laps", [{"session_key":200,"lap_number":1,"lap_duration":90}])
        anterior=dict(self.db.execute("SELECT * FROM vueltas").fetchone())
        for datos in [p.ErrorActualizacion("red caída"),[{"session_key":999,"lap_number":2}]]:
            with self.subTest(datos=datos):
                with self.assertRaises(p.ErrorActualizacion):
                    self.descargar("laps",datos)
                self.assertEqual(dict(self.db.execute("SELECT * FROM vueltas").fetchone()),anterior)
                estado=self.db.execute("SELECT * FROM descargas WHERE endpoint='laps'").fetchone()
                self.assertEqual(estado["estado"],"error")
                self.assertEqual(estado["respuesta_id"],anterior["respuesta_id"])

    def test_respuesta_vacia_tiene_estado_y_reintento_propio(self):
        self.descargar("pit", [])
        estado=self.db.execute("SELECT * FROM descargas WHERE endpoint='pit'").fetchone()
        self.assertEqual(estado["estado"],"vacia")
        self.assertEqual(estado["filas"],0)
        firma=p._firmaSesion(self.sesion)
        self.assertFalse(p.necesitaDescarga(self.db,"pit",{"session_key":200},AHORA,firma=firma))
        self.assertTrue(p.necesitaDescarga(self.db,"pit",{"session_key":200},AHORA+timedelta(days=1),firma=firma))

    def test_vacio_404_conserva_codigo_y_cuerpo_originales(self):
        cuerpo={"detail":"No results found."}
        self.descargar("pit",p.RespuestaAPI([],404,cuerpo))
        fila=self.db.execute("SELECT codigo_http,contenido_json FROM respuestas_api WHERE endpoint='pit'").fetchone()
        self.assertEqual(fila["codigo_http"],404)
        self.assertEqual(json.loads(fila["contenido_json"]),cuerpo)
        self.assertEqual(self.db.execute("SELECT estado FROM descargas WHERE endpoint='pit'").fetchone()[0],"vacia")

    def test_reanudacion_omite_exitos_y_reintenta_errores_a_su_debido_tiempo(self):
        self.descargar("drivers", [{"session_key":200,"driver_number":7}])
        with self.assertRaises(p.ErrorActualizacion):
            self.descargar("laps",p.ErrorActualizacion("fallo temporal"))
        firma=p._firmaSesion(self.sesion)
        self.assertFalse(p.necesitaDescarga(self.db,"drivers",{"session_key":200},AHORA,firma=firma))
        self.assertFalse(p.necesitaDescarga(self.db,"laps",{"session_key":200},AHORA,firma=firma))
        self.assertTrue(p.necesitaDescarga(self.db,"laps",{"session_key":200},AHORA+timedelta(minutes=16),firma=firma))
        self.assertTrue(p.necesitaDescarga(self.db,"weather",{"session_key":200},AHORA,firma=firma))

    def test_cambio_de_fechas_y_forzar_invalidan_la_cache(self):
        self.descargar("stints", [])
        modificada={**self.sesion,"fecha_fin":"2023-03-01T12:00:00+00:00"}
        self.assertTrue(p.necesitaDescarga(self.db,"stints",{"session_key":200},AHORA,firma=p._firmaSesion(modificada)))
        self.assertTrue(p.necesitaDescarga(self.db,"stints",{"session_key":200},AHORA,True,p._firmaSesion(self.sesion)))

    def test_revision_conserva_respuesta_anterior_y_no_duplica_snapshot_identico(self):
        datos=[{"session_key":200,"driver_number":7,"full_name":"NOMBRE SINTÉTICO"}]
        self.descargar("drivers",datos)
        self.descargar("drivers",datos)
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM respuestas_api WHERE endpoint='drivers'").fetchone()[0],1)
        self.descargar("drivers",[{**datos[0],"full_name":"NOMBRE REVISADO SINTÉTICO"}])
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM respuestas_api WHERE endpoint='drivers'").fetchone()[0],2)
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM pilotos").fetchone()[0],1)

    def test_catalogo_invalido_no_desactiva_registros_validos(self):
        cliente=ClienteFalso(self.db,{"meetings":[{**GP,"year":2024}]})
        with self.assertRaises(p.ErrorActualizacion):
            p.descargarYGuardar(cliente,"meetings",{"year":2023},AHORA)
        self.assertEqual(self.db.execute("SELECT presente_en_fuente FROM grandes_premios").fetchone()[0],1)

    def test_fila_que_desaparece_del_catalogo_no_borra_sus_datos(self):
        extra={**SESION,"session_key":201}
        cliente=ClienteFalso(self.db,{"sessions":[SESION,extra]})
        with patch.object(p,"ahoraUTC",return_value=AHORA):
            p.descargarYGuardar(cliente,"sessions",{"year":2023},AHORA)
            cliente.datos["sessions"]=[SESION]
            p.descargarYGuardar(cliente,"sessions",{"year":2023},AHORA)
        self.assertEqual(self.db.execute("SELECT presente_en_fuente FROM sesiones WHERE session_key=201").fetchone()[0],0)
        self.assertEqual(self.db.execute("SELECT COUNT(*) FROM sesiones").fetchone()[0],2)

    def test_estado_no_consulta_red(self):
        with patch.object(p,"urlopen") as red, redirect_stdout(StringIO()) as salida:
            p.mostrarEstado(self.db)
        red.assert_not_called()
        self.assertIn("grandes_premios: 1",salida.getvalue())


class PruebasMigracion(unittest.TestCase):
    def test_migracion_invalida_se_revierte_completa(self):
        with closing(p.abrirBase(":memory:")) as db:
            db.execute("PRAGMA foreign_keys=OFF")
            db.execute("CREATE TABLE grandes_premios (meeting_key INTEGER PRIMARY KEY,temporada INTEGER NOT NULL,nombre TEXT NOT NULL,fecha_inicio TEXT NOT NULL,actualizado_en TEXT NOT NULL)")
            db.execute("CREATE TABLE sesiones (session_key INTEGER PRIMARY KEY,meeting_key INTEGER NOT NULL REFERENCES grandes_premios(meeting_key),nombre TEXT NOT NULL,fecha_inicio TEXT NOT NULL,fecha_fin TEXT NOT NULL,actualizado_en TEXT NOT NULL)")
            db.execute("INSERT INTO sesiones VALUES (200,999,'Practice 1','2023-01-01','2023-01-02','2023-01-03')")
            db.commit()
            db.execute("PRAGMA foreign_keys=ON")
            with self.assertRaises(p.ErrorActualizacion):
                p.crearTablas(db)
            self.assertEqual(db.execute("SELECT meeting_key FROM sesiones").fetchone()[0],999)
            self.assertNotIn("tipo",{r[1] for r in db.execute("PRAGMA table_info(sesiones)")})
            self.assertFalse(db.execute("SELECT name FROM sqlite_master WHERE name LIKE '%_nueva'").fetchall())

    def test_migra_base_anterior_sin_perder_claves_ni_fechas_y_puede_repetirse(self):
        with closing(p.abrirBase(":memory:")) as db:
            db.execute("CREATE TABLE grandes_premios (meeting_key INTEGER PRIMARY KEY,temporada INTEGER NOT NULL,nombre TEXT NOT NULL,fecha_inicio TEXT NOT NULL,actualizado_en TEXT NOT NULL)")
            db.execute("CREATE TABLE sesiones (session_key INTEGER PRIMARY KEY,meeting_key INTEGER NOT NULL REFERENCES grandes_premios(meeting_key),nombre TEXT NOT NULL,fecha_inicio TEXT NOT NULL,fecha_fin TEXT NOT NULL,actualizado_en TEXT NOT NULL)")
            db.execute("INSERT INTO grandes_premios VALUES (100,2023,'SINTETICO','2023-01-01','2023-01-02')")
            db.execute("INSERT INTO sesiones VALUES (200,100,'Practice 1','2023-01-01','2023-01-02','2023-01-03')")
            db.commit()
            p.crearTablas(db)
            p.crearTablas(db)
            self.assertEqual(db.execute("SELECT fecha_inicio FROM grandes_premios").fetchone()[0],"2023-01-01")
            self.assertEqual(db.execute("SELECT session_key FROM sesiones").fetchone()[0],200)
            self.assertFalse(db.execute("PRAGMA foreign_key_check").fetchall())
            self.assertEqual(db.execute("PRAGMA user_version").fetchone()[0],p.VERSION_BASE)

    def test_respaldo_automatico_conserva_la_base_original(self):
        with tempfile.TemporaryDirectory() as carpeta:
            ruta=Path(carpeta)/"datos.sqlite3"
            with closing(p.abrirBase(ruta)) as db:
                db.execute("CREATE TABLE grandes_premios (meeting_key INTEGER PRIMARY KEY)")
                db.execute("INSERT INTO grandes_premios VALUES (100)")
                db.commit()
                with redirect_stdout(StringIO()):
                    p.respaldarAntesDeMigrar(db,ruta)
            copias=list(Path(carpeta).glob('*_antes_migracion_*.sqlite3'))
            self.assertEqual(len(copias),1)
            with closing(sqlite3.connect(copias[0])) as copia:
                self.assertEqual(copia.execute("SELECT meeting_key FROM grandes_premios").fetchone()[0],100)

    def test_version_futura_no_se_modifica(self):
        with closing(p.abrirBase(":memory:")) as db:
            db.execute("PRAGMA user_version=999")
            with self.assertRaises(p.ErrorActualizacion):
                p.crearTablas(db)
            self.assertEqual(db.execute("PRAGMA user_version").fetchone()[0],999)


class PruebasHTTP(unittest.TestCase):
    def setUp(self):
        self.db=p.abrirBase(":memory:")
        p.crearTablas(self.db)
        self.cliente=p.ClienteOpenF1(self.db)

    def tearDown(self):
        self.db.close()

    def test_rate_limit_persiste_y_se_comparte_entre_clientes(self):
        otro=p.ClienteOpenF1(self.db)
        with patch.object(p.time,"time",return_value=100),patch.object(p.time,"sleep") as dormir:
            self.cliente.esperarTurno()
            otro.esperarTurno()
        self.assertAlmostEqual(dormir.call_args_list[1].args[0],2.1)
        self.assertAlmostEqual(self.db.execute("SELECT valor FROM control_actualizador").fetchone()[0],104.2)

    def test_429_respeta_retry_after(self):
        respuesta=MagicMock()
        respuesta.__enter__.return_value=StringIO('[{"session_key":200}]')
        error=HTTPError("url",429,"límite",{"Retry-After":"8"},None)
        with patch.object(self.cliente,"esperarTurno"),patch.object(p,"urlopen",side_effect=[error,respuesta]),patch.object(p.time,"sleep") as dormir,redirect_stdout(StringIO()):
            self.assertEqual(self.cliente.obtener("weather",{"session_key":200}),[{"session_key":200}])
        dormir.assert_any_call(8.0)
        self.assertEqual(self.cliente.consultas,2)

    def test_403_no_se_interpreta_como_datos_vacios(self):
        error=HTTPError("url",403,"denegado",{},None)
        with patch.object(self.cliente,"esperarTurno"),patch.object(p,"urlopen",side_effect=error):
            with self.assertRaisesRegex(p.ErrorActualizacion,"HTTP 403"):
                self.cliente.obtener("laps",{"session_key":200})
        self.assertEqual(self.cliente.consultas,1)

    def test_404_solo_es_vacio_si_el_servidor_indica_no_results_found(self):
        for cuerpo,vacio in [(b'{"detail":"No results found."}',True),
                            (b'{"detail":"Not Found"}',False)]:
            error=HTTPError("url",404,"no encontrado",{},BytesIO(cuerpo))
            with self.subTest(cuerpo=cuerpo),patch.object(self.cliente,"esperarTurno"),patch.object(p,"urlopen",side_effect=error):
                if vacio:
                    resultado=self.cliente.obtener("pit",{"session_key":200})
                    self.assertEqual(resultado,[])
                    self.assertEqual(resultado.codigoHTTP,404)
                    self.assertEqual(resultado.original,{"detail":"No results found."})
                else:
                    with self.assertRaises(p.ErrorActualizacion):
                        self.cliente.obtener("pit",{"session_key":200})

    def test_respuesta_malformada_no_es_una_lista_vacia(self):
        respuesta=MagicMock()
        respuesta.__enter__.return_value=StringIO('{"error":"SINTÉTICO"}')
        with patch.object(self.cliente,"esperarTurno"),patch.object(p,"urlopen",return_value=respuesta):
            with self.assertRaises(p.ErrorActualizacion):
                self.cliente.obtener("laps",{"session_key":200})


class PruebaProgramaCompleto(unittest.TestCase):
    def test_ejecucion_y_reanudacion_sin_repetir_descargas(self):
        registros={
            "meetings":[GP],"sessions":[SESION],
            "drivers":[{"session_key":200,"driver_number":7,"full_name":"PILOTO SINTÉTICO"}],
            "weather":[{"session_key":200,"date":"2023-03-01T10:00:00Z","rainfall":1}],
            "laps":[{"session_key":200,"driver_number":7,"lap_number":1,"lap_duration":90}],
            "stints":[{"session_key":200,"driver_number":7,"stint_number":1,"compound":"MEDIUM","tyre_age_at_start":None}],
            "pit":[],"race_control":[{"session_key":200,"message":"MENSAJE SINTÉTICO"}],
        }
        def responder(solicitud,**kwargs):
            endpoint=solicitud.full_url.split('?')[0].rsplit('/',1)[1]
            respuesta=MagicMock()
            respuesta.__enter__.return_value=StringIO(json.dumps(registros[endpoint]))
            return respuesta
        with tempfile.TemporaryDirectory() as carpeta:
            ruta=Path(carpeta)/"prueba.sqlite3"
            args=["--base",str(ruta),"--desde","2023","--hasta","2023","--max-sesiones","1"]
            with patch.object(p,"urlopen",side_effect=responder) as red,patch.object(p.time,"sleep"),redirect_stdout(StringIO()):
                self.assertEqual(p.main(args),0)
                self.assertEqual(red.call_count,8)
                self.assertEqual(p.main(args),0)
                self.assertEqual(red.call_count,8)
            with closing(sqlite3.connect(ruta)) as db:
                self.assertEqual(db.execute("SELECT COUNT(*) FROM descargas").fetchone()[0],8)
                self.assertEqual(db.execute("SELECT rainfall FROM clima").fetchone()[0],1)
                self.assertIsNone(db.execute("SELECT tyre_age_at_start FROM stints").fetchone()[0])
            with patch.object(p,"urlopen") as red,redirect_stdout(StringIO()):
                self.assertEqual(p.main(["--base",str(ruta),"--estado"]),0)
                red.assert_not_called()


if __name__ == "__main__":
    unittest.main()
