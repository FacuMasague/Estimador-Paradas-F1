"""Guardar y actualizar el catálogo de reuniones de OpenF1 en una base SQLite.

Esta etapa consulta temporadas desde 2023 hasta el año actual. El acceso al
histórico es gratuito y no requiere API key. El catálogo puede incluir pruebas
y reuniones futuras: todavía no filtra prácticas utilizables ni estima estrategias.
"""

import time
from datetime import datetime, timezone
import sqlite3
from pathlib import Path
import json
import ssl
import certifi
from urllib.request import urlopen


def abrirBase():
    """Abrir la base local y devolver su conexión; crear el archivo si no existe."""
    # La ruta se construye junto al script, no desde la carpeta de la terminal.
    rutaBase = Path(__file__).parent / "datosOpenF1.sqlite3"
    conexion = sqlite3.connect(rutaBase)

    # SQLite requiere activar este control en cada conexion.
    conexion.execute("PRAGMA foreign_keys = ON")

    return conexion


def crearTablas(conexion):
    """Crear la tabla del catálogo si todavía no existe, sin borrar sus datos."""
    # meeting_key es el identificador de la fuente y no se puede repetir.
    # NOT NULL exige un valor. Las fechas se guardan como texto en formato ISO.
    conexion.execute("""
    CREATE TABLE IF NOT EXISTS grandes_premios (
        meeting_key INTEGER PRIMARY KEY,
        temporada INTEGER NOT NULL,
        nombre TEXT NOT NULL,
        fecha_inicio TEXT NOT NULL,
        actualizado_en TEXT NOT NULL
        )
    """)

    conexion.execute("""
    CREATE TABLE IF NOT EXISTS sesiones (
            session_key INTEGER PRIMARY KEY,
            meeting_key INTEGER NOT NULL,
            nombre TEXT NOT NULL,
            fecha_inicio TEXT NOT NULL,
            fecha_fin TEXT NOT NULL,
            actualizado_en TEXT NOT NULL,
            FOREIGN KEY (meeting_key)
                REFERENCES grandes_premios(meeting_key)
        )
    """)

def obtenerClavesGuardadas(conexion):
    """Devolver los identificadores de las reuniones que ya estan en la base."""
    resultado = conexion.execute("""
        SELECT meeting_key
        FROM grandes_premios
    """)

    clavesGuardadas = {fila[0] for fila in resultado.fetchall()}

    return clavesGuardadas

def consultarGrandesPremios(temporada):
    """Recibir un año y devolver la lista de reuniones obtenida de OpenF1."""
    # El signo ? inicia los parámetros; year limita la consulta a una temporada.
    url = f"https://api.openf1.org/v1/meetings?year={temporada}"
    # certifi aporta certificados para verificar la conexión HTTPS con la fuente.
    contextoSSL = ssl.create_default_context(cafile=certifi.where())
    with urlopen(url, context=contextoSSL, timeout=30) as respuesta:
        # Convertir el JSON recibido en una lista de diccionarios de Python.
        grandesPremios = json.load(respuesta)

    return grandesPremios

def consultarSesiones(temporada):
    """Obtener las sesiones registradas por OpenF1 para una temporada."""
    url = f"https://api.openf1.org/v1/sessions?year={temporada}"
    contextoSSL = ssl.create_default_context(cafile=certifi.where())

    with urlopen(url, context=contextoSSL, timeout=30) as respuesta:
        sesiones = json.load(respuesta)

    return sesiones

def tieneTresPracticasTerminadas(meetingKey, sesiones, ahora):
    """Comprobar si una reunion cumple el requisito temporal de las tres prácticas."""
    practicasNecesarias = {"Practice 1", "Practice 2", "Practice 3"}
    practicasTerminadas = set()

    for sesion in sesiones:
        # Revisar solamente las sesiones de la reunion indicada.
        if sesion.get("meeting_key") != meetingKey:
            continue

        nombre = sesion.get("session_name")

        # Los fines de semana sprint quedan fuera del alcance del proyecto.
        if "sprint" in nombre.lower():
            return False
        
        if nombre not in practicasNecesarias:
            continue

        # Exigir que la fuente indique explicitamente que no esta cancelada.
        if sesion.get("is_cancelled") is not False:
            continue

        textoFecha = sesion.get("date_end")
        if not isinstance(textoFecha, str):
            continue
        try:
            fechaFin = datetime.fromisoformat(
                textoFecha.replace("Z", "+00:00")
            )
        except ValueError:
            continue

        # Sin zona horaria no se pueden comparar los instantes con seguridad.
        if fechaFin.tzinfo is None:
            continue

        if fechaFin <= ahora:
            practicasTerminadas.add(nombre)

    return practicasNecesarias.issubset(practicasTerminadas)

def guardarGranPremio(conexion, granPremio):
    """Insertar una reunión o actualizarla si su meeting_key ya está guardado."""
    # Esta fecha UTC registra nuestro guardado, no la fecha de modificación de OpenF1.
    actualizacion = datetime.now(timezone.utc).isoformat()

    # Los ? reciben los valores de la tupla que sigue al texto SQL.
    # ON CONFLICT evita duplicados; excluded contiene los nuevos valores recibidos.
    conexion.execute("""
        INSERT INTO grandes_premios (
            meeting_key,
            temporada,
            nombre,
            fecha_inicio,
            actualizado_en
        )
        VALUES (?, ?, ?, ?, ?)
        ON CONFLICT(meeting_key) DO UPDATE SET
            temporada = excluded.temporada,
            nombre = excluded.nombre,
            fecha_inicio = excluded.fecha_inicio,
            actualizado_en = excluded.actualizado_en
    """, (
        granPremio["meeting_key"],
        granPremio["year"],
        granPremio["meeting_name"],
        granPremio["date_start"],
        actualizacion
    ))

    # Confirmar la escritura de este registro en el archivo de la base.
    conexion.commit()

def guardarSesion(conexion, sesion):
    """Insertar una sesion o actualizarla si ya esta guardada."""
    actualizacion = datetime.now(timezone.utc).isoformat()

    conexion.execute("""
        INSERT INTO sesiones (
            session_key,
            meeting_key,
            nombre,
            fecha_inicio,
            fecha_fin,
            actualizado_en
        )
        VALUES (?, ?, ?, ?, ?, ?)
        ON CONFLICT(session_key) DO UPDATE SET
            meeting_key = excluded.meeting_key,
            nombre = excluded.nombre,
            fecha_inicio = excluded.fecha_inicio,
            fecha_fin = excluded.fecha_fin,
            actualizado_en = excluded.actualizado_en
    """, (
        sesion["session_key"],
        sesion["meeting_key"],
        sesion["session_name"],
        sesion["date_start"],
        sesion["date_end"],
        actualizacion
        ))

    conexion.commit()

def actualizarBase(temporada):
    """Consultar y guardar el catálogo completo de una temporada."""
    # Primero obtener la respuesta: si la consulta falla, no comenzar el guardado.
    grandesPremios = consultarGrandesPremios(temporada)
    print(f"Reuniones recibidas: {len(grandesPremios)}")

    # Espacias las consultas al catalogo y a las sesiones.
    time.sleep(2.1)

    sesiones = consultarSesiones(temporada)
    ahora = datetime.now(timezone.utc)

    conexion = abrirBase()
    crearTablas(conexion)

    clavesGuardadas = obtenerClavesGuardadas(conexion)
    cantidadNuevas = 0
    cantidadExistentes = 0
    cantidadDescartadas = 0

    for granPremio in grandesPremios:
        clave = granPremio["meeting_key"]

        # Excluir reuniones marcadas como canceladas por la fuente.
        if granPremio.get("is_cancelled") is True:
            cantidadDescartadas += 1
            continue

        # No guardar reuniones que todavía no cumplen el requisito.
        if not tieneTresPracticasTerminadas(clave, sesiones, ahora):
            cantidadDescartadas += 1
            continue

        if clave in clavesGuardadas:
            cantidadExistentes += 1
        else:
            cantidadNuevas += 1
            print("Nueva reunión:", granPremio["meeting_name"])

        # Guardar primero el Gran Premio para que exista la referencia.
        guardarGranPremio(conexion, granPremio)
        clavesGuardadas.add(clave)

        # Guardas las practicas correspondientes al Gran Premio aceptado.
        for sesion in sesiones:
            perteneceAlGranPremio = sesion.get("meeting_key") == clave
            esPractica = sesion.get("session_name") in {
                "Practice 1",
                "Practice 2",
                "Practice 3"
            }
            noEstaCancelada = sesion.get("is_cancelled") is False

            if noEstaCancelada and perteneceAlGranPremio and esPractica:
                guardarSesion(conexion, sesion)

    print(f"Reuniones nuevas guardadas: {cantidadNuevas}")
    print(f"Reuniones ya existentes actualizadas: {cantidadExistentes}")
    print(f"Reuniones descartadas por el filtro: {cantidadDescartadas}")

    # Contar todas las reuniones de la base, incluidas las de otras temporadas.
    resultado = conexion.execute("""
        SELECT COUNT(*)
        FROM grandes_premios
    """)

    # fetchone() devuelve una fila; [0] obtiene el único valor, el conteo.
    cantidadGuardada = resultado.fetchone()[0]
    print(f"\nTotal de reuniones en la base: {cantidadGuardada}")

    conexion.close()


# Ejecutar este recorrido solo al iniciar el archivo como programa, no al importarlo.
if __name__ == "__main__":
    # OpenF1 ofrece históricos desde 2023. El año final se obtiene del reloj local.
    temporadaInicial = 2023
    temporadaActual = datetime.now().year

    # range excluye el límite final; sumar 1 permite incluir el año actual.
    for temporada in range(temporadaInicial, temporadaActual + 1):
        if temporada > temporadaInicial:
            # Espaciar las consultas entre temporadas para respetar el acceso gratuito.
            time.sleep(2.1)

        # Por ahora se consulta cada año completo, incluso si ya estaba guardado.
        print(f"\nActualizando temporada {temporada}...")
        actualizarBase(temporada)
