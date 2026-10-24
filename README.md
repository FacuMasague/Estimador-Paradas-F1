# Estimador de paradas de F1 — actualizador de datos

Proyecto universitario de Metodología del Aprendizaje. **Python solo descarga y
actualiza una base SQLite. El análisis y las estrategias se desarrollarán en R
con RStudio.** No hay interfaz de selección de piloto ni modelo de ritmo en Python.

Todo el código de funcionamiento está en `estimadorF1.py`. La base se llama
`datosOpenF1.sqlite3` y queda junto al script, independientemente de la carpeta
desde donde se lo ejecute. OpenF1 histórico no necesita API key ni suscripción.

## Instalación

Requiere Python 3.10 o posterior. Para trabajar con un entorno propio del proyecto:

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install -r requirements.txt
```

En Windows la activación desde PowerShell es `.venv\Scripts\Activate.ps1`.
En VS Code seleccionar `.venv` mediante **Python: Select Interpreter**. Se evita
así utilizar un Python distinto del que tiene instaladas las dependencias.
El script usa certifi si está disponible; en caso contrario utiliza el almacén de
certificados de Python/sistema. Siempre verifica TLS.

## Ejecutar y reanudar

```bash
python estimadorF1.py
```

La ejecución normal revisa el catálogo del año actual, completa temporadas cuyo
catálogo todavía falta y descarga conjuntos pendientes de sesiones terminadas.
Por defecto obtiene prácticas y carreras desde 2023 hasta el año actual, con:

- pilotos (`drivers`);
- clima (`weather`);
- vueltas (`laps`);
- stints (`stints`);
- boxes (`pit`);
- mensajes de control de carrera (`race_control`).

**La primera descarga de todo el historial puede tardar horas.** Cada sesión puede
requerir seis solicitudes; el programa las espacia al menos 2.1 segundos. No se
consulta telemetría. Ctrl+C conserva las descargas ya confirmadas y la siguiente
ejecución retoma los conjuntos pendientes.

Para comenzar con lotes pequeños:

```bash
python estimadorF1.py --max-sesiones 3
```

Repetir ese comando completa hasta tres sesiones pendientes más. Un conjunto
vacío o fallido espera su plazo de reintento y no bloquea los restantes.

Otros ejemplos:

```bash
# Solo catálogo: reuniones y sesiones; no descarga sus registros de detalle.
python estimadorF1.py --solo-catalogo

# Ver lo guardado sin red ni migraciones.
python estimadorF1.py --estado

# Una práctica real: Bahréin 2023, Practice 1.
python estimadorF1.py --desde 2023 --hasta 2023 --sesion 7765

# Solo prácticas, o solo los conjuntos que se van a utilizar primero.
python estimadorF1.py --tipos Practice --endpoints drivers weather laps stints --max-sesiones 3

# Reconsultar catálogos de años anteriores para detectar correcciones tardías.
python estimadorF1.py --revisar-catalogo --solo-catalogo

# Reconsultar los datos de una sesión, incluso si estaban guardados.
python estimadorF1.py --desde 2023 --hasta 2023 --sesion 7765 --forzar

# Base independiente para una prueba.
python estimadorF1.py --base prueba.sqlite3 --desde 2023 --hasta 2023 --max-sesiones 1

python estimadorF1.py --help
```

`--sesion` se puede repetir. Se aplica al detalle; el catálogo se mantiene para
el rango de temporadas solicitado. `--tipos` se compara con el tipo publicado por
OpenF1 (`Practice`, `Race`, etc.). Posiciones e intervalos son opcionales mediante
`--endpoints position intervals`; `intervals` se consulta solo para carreras.

## Disponibilidad y responsabilidad de R

El catálogo conserva reuniones y sesiones **también si son futuras, canceladas,
pruebas de pretemporada o pertenecen a un fin de semana sprint**. Sirve como
registro de la fuente, no como una lista de casos admisibles.

Python descarga detalles solo cuando conoce inicio y fin con zona horaria, la
sesión no está marcada como cancelada ni su reunión cancelada, y terminó hace
al menos 60 minutos. `--espera-minutos` permite configurar ese margen; no garantiza
que la fuente ya haya publicado todos los datos y no habilita acceso en vivo.

Las reglas del proyecto se aplicarán en R: tres prácticas terminadas, exclusión
de sprint, inicio seco utilizable, pilotos, compuestos y datos suficientes. Python
no interpreta lluvia, no elimina vueltas lentas ni neutralizadas y no ajusta ritmo.
Los mensajes de control se guardan como mensajes: no se cuentan como episodios
VSC/SC ni se les asignan probabilidades.

En R, los datos de la carrera objetivo deben reservarse para evaluación y no
usarse para ajustar una recomendación previa a su largada. El actualizador permite
guardar carreras históricas y prácticas; su presencia conjunta en SQLite no autoriza
mezclarlas. Guardar hoy un registro histórico tampoco demuestra que su versión
actual ya estuviera disponible antes de aquella carrera.

## Actualizaciones y respuestas de la fuente

- Los catálogos del año actual se revisan en cada ejecución.
- Los catálogos de temporadas anteriores se reutilizan tras descargarlos; usar
  `--revisar-catalogo` para volver a verificarlos.
- Los conjuntos válidos de sesiones consolidadas se reutilizan. Un cambio en las
  fechas, tipo o cancelación de la sesión obliga a revisarlos.
- Para sesiones terminadas hace menos de tres días, se revisan cada seis horas.
- Respuestas vacías se identifican como `vacia` y se reconsultan en 24 horas.
- OpenF1 también responde HTTP 404 con `{"detail":"No results found."}` cuando
  no encuentra registros. Solo ese mensaje exacto se trata como vacío. Se guardan
  el código HTTP y su cuerpo original; otros 404 son errores. Esto no demuestra
  que no hubiera pasos por boxes: puede faltar cobertura de la fuente.
- Errores se identifican como `error`, conservan la última descarga válida y se
  reintentan después de 15 minutos. `--forzar` permite pedir una revisión inmediata.
- `--forzar` reconsulta también el catálogo del rango solicitado. No requiere
  borrar la base.
- Los HTTP 429 y algunos errores temporales del servidor se reintentan hasta tres
  veces, respetando `Retry-After`. Una interrupción no se registra como dato vacío.

La separación de solicitudes se comparte entre procesos que utilicen la misma
base. No coordina otras aplicaciones o bases que consulten desde la misma IP.

## Estructura de SQLite

| Tabla | Contenido y relación principal |
| --- | --- |
| `grandes_premios` | Catálogo. Clave `meeting_key`; incluye temporada y circuito. |
| `sesiones` | Catálogo. Clave `session_key`; referencia `meeting_key`. |
| `pilotos` | Participantes por sesión; no demuestra disponibilidad de juegos para carrera. |
| `clima` | Observaciones meteorológicas por sesión. |
| `vueltas` | Vueltas observadas, con tiempos, sectores y pit-out. |
| `stints` | Compuesto, límites de tramo y edad inicial informados. |
| `boxes` | Registros de paso por boxes. |
| `control_carrera` | Mensajes y banderas de la fuente, sin reconstrucción de episodios. |
| `posiciones` / `intervalos` | Conjuntos opcionales de tráfico observado. |
| `descargas` | Estado por endpoint y parámetros; fechas de intento, éxito y revisión. |
| `respuestas_api` | Respuestas originales en JSON con fecha y huella SHA-256. |
| `control_actualizador` | Reserva del siguiente turno de consulta. |

Las columnas del catálogo conservan los nombres en español que ya tenía el
proyecto. En las tablas de detalle se conservan los nombres de los campos de
OpenF1 para facilitar su trazabilidad. Los campos escalares son columnas normales,
no hace falta analizar JSON para leer vueltas o clima desde R.

En detalle, la clave técnica es **`(session_key, registro)`**. `registro` es la
posición, desde 1, dentro de la respuesta original: no es el número de vuelta ni
un identificador de evento deportivo. Se mantienen duplicados de la fuente para
que R decida cómo tratarlos. Una respuesta completa reemplaza atómicamente la foto
actual de ese conjunto/sesión. Las versiones originales diferentes se conservan
en `respuestas_api`; respuestas idénticas no duplican el JSON.

`respuesta_id` vincula los datos con su respuesta original. Los campos nuevos de
OpenF1 que todavía no tengan columna permanecen en ese JSON. Campos ausentes se
guardan como `NULL`: no se sustituye una edad desconocida por cero ni una lluvia
desconocida por ausencia de lluvia. Las listas de mini-sectores se guardan como
texto JSON en sus respectivas columnas.

`respuestas_api.codigo_http` permite distinguir un JSON de registros HTTP 200 de
un mensaje explícito de ausencia de resultados HTTP 404. El JSON de este último
es un objeto con `detail`, no una lista. Ambos quedan disponibles para auditoría.

`presente_en_fuente=0` señala un registro que desapareció de un catálogo actualizado;
no se borran sus sesiones ni los datos ya descargados. Las respuestas vacías de
catálogo no desactivan el catálogo anterior. `cancelado`/`cancelada` pueden ser
`NULL` si todavía no se conoce el dato. `actualizado_en` es nuestro momento de
guardado en UTC, no una fecha de revisión publicada por OpenF1.

### Fechas, unidades y convenciones para R

- Metadatos de inicio/fin se normalizan a ISO 8601 con zona UTC. Los timestamps de
  detalle se conservan tal como los publica la fuente; interpretarlos con zona.
- Duraciones de vuelta, sectores y boxes: segundos.
- `tyre_age_at_start`: vueltas completadas antes de comenzar el stint. No calcula
  el porcentaje de desgaste ni la edad por vuelta; eso corresponde a R.
- Temperaturas: °C; presión: mbar; viento: m/s y dirección en grados; humedad: %.
- Booleanos: 0/1, con `NULL` para desconocidos. `rainfall` indica precipitación;
  no certifica que el asfalto esté seco.
- `lane_duration` y `stop_duration` se guardan por separado. No se calcula pérdida
  adicional por boxes. `pit_duration` se conserva cuando la fuente lo entrega,
  sin usarlo como relleno de otro campo.
- `interval` y `gap_to_leader` se guardan como texto para conservar tanto segundos
  como expresiones como `+1 LAP`; convertir y validar en R según el caso.

La disponibilidad varía por sesión y campo. En particular, `stop_duration` no
cubre toda la historia desde 2023. El contrato de la fuente está en la
[documentación de OpenF1](https://openf1.org/docs/).

### Consultas de inspección

En DB Browser, o posteriormente desde DBI/RSQLite:

```sql
SELECT gp.temporada, gp.nombre, s.session_key, s.nombre AS sesion,
       s.tipo, s.fecha_fin, s.cancelada
FROM sesiones s
JOIN grandes_premios gp ON gp.meeting_key = s.meeting_key
ORDER BY s.fecha_inicio;

SELECT session_key, driver_number, lap_number, lap_duration
FROM vueltas
ORDER BY session_key, driver_number, lap_number
LIMIT 20;

SELECT endpoint, estado, filas, exito_en, proxima_revision, error
FROM descargas
ORDER BY endpoint, parametros_json;
```

Abrir SQLite desde R en modo lectura una vez completado el lote de actualización.
No se creó un programa R: esa parte se desarrollará paso a paso con el autor.

## Migración de la base anterior

No borrar la base del tutorial. En el primer inicio, el actualizador crea un
respaldo `datosOpenF1_antes_migracion_FECHA.sqlite3` y conserva los identificadores
y registros existentes al ampliar la estructura. Agrega tipos de sesión, estados
de cancelación, fecha final de reunión y trazabilidad.

Algunos campos de registros migrados son inicialmente `NULL` hasta consultar su
catálogo original. `PRAGMA user_version` identifica la versión de estructura.
No modificarlo manualmente. Una base con versión posterior a la del script se
rechaza para evitar una migración equivocada.

## Pruebas

```bash
python -m unittest -v test_catalogo
```

Las pruebas usan registros sintéticos y bases temporales/en memoria. Comprueban
migración, reanudación, estados, errores, snapshots, duplicados, campos faltantes y
límite de consultas. No miden precisión de estrategias ni rendimiento predictivo.
La salida del programa es 0 al completar sin errores registrados, 1 ante errores
o solicitudes no disponibles, y 130 si se interrumpe con Ctrl+C. Vacíos identificados
no se confunden con errores; consultar siempre los estados y la cobertura desde R.

`estimadorF1chat.py`, `casos_sinteticos.json`, `test_estimadorF1.py` y
`notas_diseno_anteriores.md` se conservan como antecedentes y no intervienen en la
actualización. Las notas antiguas no definen el alcance vigente.

## GitHub y alcance de fuentes

Subir `estimadorF1.py`, `requirements.txt`, `README.md`, `test_catalogo.py` y
`.gitignore`. Las bases, respaldos, entornos y configuración local del editor
quedan fuera de Git. Cada integrante genera su base con el mismo script.

Esta versión implementa OpenF1 desde 2023. FastF1 no está integrado todavía;
incorporarlo requerirá distinguir fuente, cobertura y claves antes de combinar
registros. No se afirma disponibilidad de datos que no se descargaron.
