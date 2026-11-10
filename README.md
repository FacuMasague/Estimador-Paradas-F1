# Estimador de paradas de F1 — actualizador de datos

Proyecto universitario de Metodología del Aprendizaje. **Python solo descarga y
actualiza una base SQLite. El análisis y las estrategias se desarrollarán en R
con RStudio.** No hay interfaz de selección de piloto ni modelo de ritmo en Python.

El actualizador está en `estimadorF1.py`; el análisis se desarrolla en
`estimadorR.R`. La base se llama
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

## Análisis en R (MVP)

Abrir `Estimador de paradas F1.Rproj` en RStudio y hacer **Source** de
`estimadorR.R`. Las fórmulas están en `modeloEstrategias.R`, que se carga solo.
Con `Rscript estimadorR.R` se omiten los menús y no se analiza ningún caso.

El programa pide primero el modo:

- **Investigación:** solo carreras secas (también antes de largar), sin bandera
  roja ni SC/VSC, y pilotos que terminaron con 1 o 2 paradas y compuestos conocidos.
  Ordena primero a los pilotos con posición media en el top 10 en sus 5 carreras
  anteriores (orden de llegada reconstruido desde las vueltas, sin penalizaciones).
  Compara el resultado con la carrera real y guarda gráficos en `resultados/`.
- **Uso:** solo exige datos suficientes de prácticas; no mira la carrera.

Modelo: `t = base_c + pendiente_c · edad + combustible`.

- **Combustible:** la carga de práctica no se publica; se estima la mínima de cada
  stint (vueltas que le faltan, contando salida y entrada, más una vuelta de margen).
  El efecto total en carrera es 3,3 s (lo que gana el auto entre la primera y la
  última vuelta por combustible y evolución de la pista), medido con una regresión
  en 54 GP, casos incluidos (3,1 s sin ellos; parecido en 2023–25 y 2026). Por vuelta
  son 3,3 / vueltas de carrera.
  Un stint de SOFT de hasta 6 vueltas se toma como simulacro de clasificación y
  consume 1,25 veces más (estimación de baja confianza).
- **Degradación:** porcentual, con todos los pilotos del fin de semana (cada uno
  aporta si tiene al menos 8 vueltas aptas con 5 vueltas de rango en ese
  compuesto; se usa la mediana y se exigen 3 pilotos).
- **Base:** la del piloto (al menos 3 vueltas aptas propias). Si le falta SOFT o
  HARD: su MEDIUM por la diferencia pareada con el MEDIUM de los pilotos que usaron
  ambos (al menos 3); si no, la de su compañero; si es el HARD y no hay nada, igual
  al MEDIUM (escenario condicionado). Sin degradación de HARD en el campo se usa la
  del MEDIUM. El programa informa el origen de cada valor.

- **Calibración práctica → carrera** (`parametros$calibracion`; medida el 10/11/2026
  con los 54 GP secos de 2023–2026 de tres prácticas, **incluidos los casos de
  investigación**: sus resultados quedan dentro de la muestra; la carrera sale de
  una regresión por GP que separa combustible y degradación; scripts y datos en
  `resultados/regresionCarrera.R`, `resultados/calibracion2.R` y
  `resultados/calibracion2.csv`):
  - la degradación de prácticas se multiplica por 0,44 (SOFT), 0,58 (MEDIUM) y 0,69
    (HARD);
  - la diferencia de base entre compuestos en prácticas no se relaciona con la de
    carrera: SOFT y HARD salen del MEDIUM propio con la diferencia mediana de carrera
    (−0,01 % y +0,03 %);
  - si el campo no alcanza para la degradación del SOFT o del HARD, se usa la del
    MEDIUM por 0,93 o 0,89 (cociente mediano en carrera).
  - el ritmo de carrera es más rápido que el de prácticas: la base se multiplica por
    0,9888 (−1,12 %; mediana de 50 GP de tres prácticas, casos incluidos, IC 95 %
    −1,61 a −0,71; sin los casos, −1,01 %). Es un ajuste promedio: la causa no se
    pudo aislar y varía mucho entre GP y pilotos (`resultados/errorNivel2.R`).
  Con `calibracion = NULL` se usa el modelo sin calibrar descripto arriba.
  Evaluación externa: los GP de Estados Unidos, México y Brasil 2026 no se descargaron
  a propósito mientras se ajustaba el modelo; son los únicos GP que el modelo no vio.

La pérdida por parada se estima con otras ediciones de la carrera del mismo circuito
(entrada + salida menos dos vueltas normales), preferentemente anteriores. Se
comparan estrategias de 1 y 2 paradas con al menos dos compuestos secos. Los
parámetros están en la lista `parametros` de `estimadorR.R`; ninguno está validado.

Edad de los juegos: el 39 % de los stints de SOFT en carrera arranca con un juego
usado (casi siempre de 3 vueltas, de clasificación), contra 12 % de MEDIUM y 10 % de
HARD (38 carreras secas que no son casos). Como no se descargan los stints de
clasificación, no se sabe qué juegos le quedan a cada piloto: se usa la edad inicial
media (1,4, 0,4 y 0,4 vueltas), que da el tiempo esperado porque el modelo es lineal
en la edad.

Margen de empate: las constantes de calibración tienen error. El programa repite la
búsqueda 200 veces sumando residuos de la calibración elegidos al azar (degradación
y diferencia de base, por compuesto) y toma como margen el percentil 90 de lo que
pierde la recomendada frente a la mejor de cada simulación. Las combinaciones a menos
de ese margen se informan como empatadas; en investigación también se informa en qué
proporción de simulaciones la estrategia real fue más rápida que la recomendada.

Gráficos de investigación: tiempos reales, predicción y recta real de cada stint
(Theil-Sen de las vueltas limpias); diferencias de estrategias con el margen de
empate; vueltas de práctica del piloto y rectas del modelo.

Caída final y vida máxima: la caída final del neumático (el "cliff" que Pirelli diseña
a partir de cierta distancia) no se pudo medir en 70 carreras (`resultados/caida.R`):
los equipos paran antes. Coincide con la literatura: el simulador de Heilmeier et al.
usa degradación lineal en sus 122 carreras ajustadas, y Cappello y Hoegh (2025) no
encuentran mejora con degradación que se acelera. En su lugar, un juego no puede
superar en carrera las vueltas que los equipos casi nunca superan (percentil 95 de
3.293 stints): 31 SOFT, 36 MEDIUM y 46 HARD (`parametros$vidaMaxima`). Es una
restricción tomada del comportamiento de los equipos, no de la física del neumático.

Safety car (detalle y resultados en `resultados/notas_neutralizaciones.md`). Para la
estrategia recomendada, el programa informa:
- la probabilidad de al menos un SC en el circuito, con carreras secas anteriores de
  OpenF1 (la frecuencia del circuito se acerca a la tasa general porque las diferencias
  entre circuitos no fueron significativas con 8 temporadas de FastF1);
- el momento típico de inicio en 5 tramos de 20 %, con todas las carreras;
- si conviene parar o seguir ante un SC que empieza en cada vuelta, resumido en 10
  secciones, con el compuesto siguiente y el ahorro estimado (4.º panel del gráfico);
- la medida D: el ahorro esperado de adaptarse frente al plan fijo, dentro del simulador.
La pérdida por parada bajo SC es 0,32 de la verde, calibrada con decisiones reales de
pilotos (82 % de acuerdo). El VSC quedó afuera de los resultados: ninguna regla superó a
"nunca parar", porque su beneficio depende de dónde está el auto cuando empieza.
Gráficos de la validación: `Rscript resultados/graficosSC.R` (o Source en RStudio).

"Mejor estrategia" significa menor tiempo dentro del modelo. No incluye tráfico,
posición en pista ni neutralizaciones, y en el modelo el orden de los stints no
cambia el tiempo.

## Actualizaciones y respuestas de la fuente

- Los catálogos del año actual se revisan en cada ejecución.
- Los catálogos de temporadas anteriores se reutilizan tras descargarlos; usar
  `--revisar-catalogo` para volver a verificarlos.
- Los conjuntos válidos de sesiones consolidadas se reutilizan. Un cambio en las
  fechas, tipo o cancelación de la sesión obliga a revisarlos.
- Para sesiones terminadas hace menos de tres días, se revisan cada seis horas.
- Respuestas vacías se identifican como `vacia`. Se reconsultan cada 24 horas solo
  durante los 7 días posteriores a la sesión; después se toman como ausencia
  definitiva de la fuente (por ejemplo, boxes de 2023) y no se vuelven a pedir.
  `--forzar` permite reintentarlas.
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
Abrir el proyecto de RStudio y ejecutar `estimadorR.R` con **Source**. Los menús
permiten elegir temporada, GP y piloto; con `Rscript` se omiten los menús.

El análisis revisa las tres prácticas del GP elegido y conserva un resumen por
sesión. Solo las prácticas con tramos meteorológicos candidatos aportan vueltas.
Las descargas ausentes, fallidas o con cantidades inconsistentes quedan señaladas
sin impedir la revisión de las otras prácticas. `revisionPiloto` contiene el
resumen y los registros; `vueltasEnTramo` conserva las vueltas cuyo inicio y final
calculado están dentro del tramo. `vueltasConStint` además exige una única
coincidencia de stint y un número de vuelta no repetido en esa sesión y piloto.

Estos conjuntos todavía requieren limpieza de boxes y neutralizaciones; no son
vueltas listas para ajustar el modelo. OpenF1 describe `date_start` como aproximado:
el final se calcula sumando `lap_duration` en segundos y también es aproximado.
Los límites `lap_start` y `lap_end` se comparan inclusivamente; una vuelta que
coincide con dos stints se marca como ambigua y no recibe compuesto automático.
Se conservan los números originales y la edad inicial informada, sin calcular
todavía la edad por vuelta ni mezclar ritmos de distintas prácticas.
Referencia: [documentación de OpenF1](https://openf1.org/docs/).

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
Rscript test_practicas.R
```

Las pruebas usan registros sintéticos y bases temporales/en memoria. Comprueban
migración, reanudación, estados, errores, snapshots, duplicados, campos faltantes y
límite de consultas. No miden precisión de estrategias ni rendimiento predictivo.
La salida del programa es 0 al completar sin errores registrados, 1 ante errores
o solicitudes no disponibles, y 130 si se interrumpe con Ctrl+C. Vacíos identificados
no se confunden con errores; consultar siempre los estados y la cobertura desde R.

Las pruebas de R usan vueltas sintéticas y una base SQLite temporal. Comprueban
el cruce de límites horarios, las asignaciones ambiguas, los duplicados y la
continuación entre prácticas cuando faltan datos. No requieren la base histórica.

`estimadorF1chat.py`, `casos_sinteticos.json`, `test_estimadorF1.py` y
`notas_diseno_anteriores.md` se conservan como antecedentes y no intervienen en la
actualización. Las notas antiguas no definen el alcance vigente.

## GitHub y alcance de fuentes

Subir `estimadorF1.py`, `estimadorR.R`, el proyecto `.Rproj`, `requirements.txt`,
`README.md`, `test_catalogo.py`, `test_practicas.R` y
`.gitignore`. Las bases, respaldos, entornos y configuración local del editor
quedan fuera de Git. Cada integrante genera su base con el mismo script.

Esta versión implementa OpenF1 desde 2023. FastF1 no está integrado todavía;
incorporarlo requerirá distinguir fuente, cobertura y claves antes de combinar
registros. No se afirma disponibilidad de datos que no se descargaron.
