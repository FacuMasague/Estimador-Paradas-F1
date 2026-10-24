# Archivo de notas anteriores

Estas notas se conservan como antecedente. El alcance vigente es Python para
actualizar SQLite y R para el análisis; consultar README.md para el uso actual.

# Estimador de paradas de F1

Proyecto universitario de Metodología del Aprendizaje. Implementación inicial
en Python; traducción a R posterior.

## Versión actual: catálogo, base local y selección

Todo el código de esta etapa está en **estimadorF1.py**. Consulta la API histórica
gratuita de OpenF1, crea **datos_openf1.sqlite3** junto al script y pide temporada,
Gran Premio y piloto. No requiere cuenta ni API key. No calcula estrategias.

Requiere Python 3.10 o posterior y usa su biblioteca estándar. Si la instalación
local de Python tiene problemas con los certificados HTTPS, instalar/actualizar
sus certificados; el script también aprovecha certifi si ya está instalado.
Nunca desactiva la verificación TLS.

```bash
python3 estimadorF1.py
```

En el primer inicio puede tardar varios minutos: el acceso gratuito está limitado
y se espacian las solicitudes 2.1 segundos. Cada respuesta se guarda inmediatamente.
Si se interrumpe con Ctrl+C, la próxima ejecución retoma lo pendiente.

```bash
python3 estimadorF1.py --solo-actualizar
python3 estimadorF1.py --sin-red
python3 estimadorF1.py --min-vueltas 8
python3 estimadorF1.py --desde 2025
python3 estimadorF1.py --help
```

En cada ejecución normal se refresca el catálogo pequeño de reuniones/sesiones
(dos consultas) para descubrir GP nuevos, cancelaciones o prácticas que terminaron.
También se revisan los huecos de descargas anteriores. Los datos históricos ya
guardados se reutilizan; las prácticas recientes se refrescan durante dos días.
Una práctica nueva modifica el corte temporal de la consulta y actualiza el GP.
Las respuestas vacías se vuelven a consultar después de una hora.
No se garantiza detectar revisiones tardías de datos históricos consolidados;
`--base otra_base.sqlite3` permite hacer una descarga independiente.

SQLite guarda endpoint, parámetros, fecha UTC de descarga y JSON original. No
contiene claves. laps y weather se consultan hasta el final de las prácticas
terminadas; de cada respuesta solo se analizan las prácticas elegibles. La caché
original de stints por reunión puede contener otras sesiones, incluida la carrera:
esas filas no se usan para la preselección. La carrera objetivo no aporta tiempos
a la selección ni se ajusta ningún modelo en esta etapa.

**Preselección provisional:** se excluyen findes sprint, tests sin carrera y
sesiones canceladas o aún no terminadas. Se exige un inicio sin lluvia registrada
y registros meteorológicos que cubran las vueltas sin huecos mayores a 120 s.
Si empieza a llover se corta el tramo; si comienza con lluvia no se recupera
la parte posterior. Se requieren slicks, stint y edad inicial conocidos,
tiempo válido, ausencia de duplicados y vuelta no identificada como pit-out.
Por defecto el piloto necesita al menos **cinco vueltas candidatas por compuesto
en dos compuestos**, sumando las prácticas elegibles. Es una regla configurable
para preseleccionar datos; no un criterio científico validado.

Todavía falta limpiar entradas a boxes, neutralizaciones, tráfico identificable
y revisar residuos, además de estudiar el rango de edades y la calidad del ajuste.
No se llama “vueltas limpias” a estas candidatas. Ausencia de lluvia y neumático
slick tampoco certifican superficie seca. La selección no garantiza todavía que
pueda emitirse una estrategia. Es posible excluir casos utilizables con evidencia
adicional porque este primer filtro es conservador.

**GitHub:** subir el script, README, .gitignore y pruebas. La base descargada queda
fuera de Git por tamaño y porque cada compañero puede generarla ejecutando el script.
No se creó repositorio ni se publicó nada. estimadorF1chat.py conserva el ejemplo
anterior y no se importa desde el programa real. Sus pruebas apuntan a ese archivo.

Verificar funcionamiento con `python3 -m unittest -v`. Las pruebas del catálogo
usan datos sintéticos y bases en memoria; no consumen consultas a OpenF1.

## Notas de diseño de la etapa anterior

Lo que sigue conserva las propuestas de asesoramiento anteriores. Las menciones
al núcleo de fórmulas corresponden al archivo de referencia **estimadorF1chat.py**,
no a funciones ya integradas en el script real. Las decisiones propuestas deben
revisarse con el autor antes de implementarlas.

## Material revisado el 21 de octubre de 2026

- Documento de referencia: [Paper Metodologia del Aprendizaje - Estimador de
  parada en boxes de F1](https://docs.google.com/document/d/1HWT4XoP8Gj4SHp7goOzE8fzCZTLRS-VF3PLdP8WClvk).
- Pestañas verificadas: Paper, TP2, TP3, TP3 2da Entrega y Programa.
- Se leyó primero Programa y luego el texto de TP3 2da Entrega, incluyendo
  Introducción, Materiales y métodos y Evaluación del diseño. La extracción de
  texto omite varias ecuaciones embebidas: sus expresiones completas quedan
  pendientes de inspección antes de implementar las métricas académicas.
- No se consultaron los antecedentes Paper, TP2 y TP3 ni se modificó Drive.
- El archivo local estimadorF1.py estaba vacío antes de este incremento.

Las instrucciones actuales del usuario prevalecen sobre el documento.

## Alcance y decisiones

Primera versión: un piloto, carrera seca, fin de semana sin sprint, sin
neutralizaciones; comparar estrategias de una y dos paradas y devolver hasta
tres alternativas admisibles con tiempos, diferencias y advertencias.
La mejor será la de menor tiempo dentro del modelo y las alternativas buscadas.

VSC/SC quedan para una etapa posterior. Los antecedentes históricos se usarán
inicialmente para contexto verificable (por ejemplo, una estimación documentada
de pérdida por boxes); no se mezclarán tiempos absolutos de temporadas distintas.
La exigencia académica de dos antecedentes se revisará para esa etapa:
dos carreras no garantizan representatividad estadística y no deben bloquear
el ejercicio sintético o un escenario con pérdida por boxes configurada.

Decisión confirmada por el usuario: permitir diferencias de ritmo configuradas
cuando no existan comparaciones sustentadas entre compuestos. Los resultados
se identificarán como escenarios condicionados; no se atribuirán esos valores
a observaciones de F1. Un compuesto sin pendiente sustentada también necesitará
un parámetro explícito de escenario o se excluirá; no se rellenará silenciosamente.

## Modelo propuesto para discutir antes del ajuste real

1. Separar prácticas y stints, asignar edades ANTES de limpiar.
2. Excluir vueltas identificadas como no representativas: boxes, interrupciones,
   neutralizaciones, incompletas y situaciones documentadas. Una vuelta lenta
   por sí sola no prueba tráfico. Conservar cada exclusión y su motivo.
3. Ajustar por stint `t_s(a) = alpha_s + d_s*a`. Interpretar d_s como variación
   observada del ritmo, afectada por combustible, pista y conducción.
4. Examinar residuos y señalar candidatos a atípicos con
   `abs(r - mediana(r)) > 3 * 1.4826 * mediana(abs(r - mediana(r)))`.
   No excluir automáticamente: revisar curvatura y cambios reales. Con pocos
   datos o MAD cero, no aplicar mecánicamente el criterio.
5. Propuesta sencilla: resumir pendientes compatibles por compuesto con su
   mediana; mostrar dispersión, número de stints y rango de edades. Si son
   incompatibles, conservar escenarios separados o excluir el compuesto.
   Con un único stint no se puede afirmar consistencia entre stints.
6. Resolver las bases por separado. Un intercepto libre por stint absorbe
   diferencias de ritmo, pero también impide identificar por sí solo la ventaja
   entre compuestos, porque cada stint utiliza un compuesto.

Para comparar compuestos, propuesta inicial centrada en una edad de referencia:

`t_c(a) = B + delta_c + d_c * (a - a_ref)`

- `a_ref`: misma edad de referencia para todos los compuestos; preferir una
  edad dentro del rango común observado si existe.
- `B`: tiempo del compuesto de referencia a esa edad, en segundos.
- `delta_c`: diferencia de tiempo respecto de esa referencia, a igual edad.
- `d_c`: segundos adicionales por vuelta de edad.

La fórmula equivale a `b_c + d_c*a` con
`b_c = B + delta_c - d_c*a_ref`. El núcleo ya admite esta última forma.
No fijamos todavía valores reales para B, delta, pendientes o límites.
Los delta configurados no se obtendrán de promedios brutos entre prácticas.
Incluso tandas cercanas del mismo piloto requieren justificar comparabilidad;
no se conoce necesariamente su carga de combustible.

Un B común suma la misma constante a todas las estrategias de igual distancia:
no cambia su orden. Sí cambia el tiempo total comunicado. Si no se dispone de
una referencia de carrera sustentada, el total será un tiempo de escenario,
no un pronóstico absoluto validado. Realizar sensibilidad a delta, d y boxes
permitirá mostrar cuándo cambia la estrategia elegida.

Pendientes negativas no se truncarán silenciosamente a cero: pueden reflejar
combustible o pista y no justifican extrapolaciones extensas.
El primer modelo omite calentamiento, largada y evolución no lineal. Un efecto
común por vuelta de carrera no cambia el orden a distancia fija; un calentamiento
por cambio de juego sí podría cambiarlo y debe constar como limitación.

## Convenciones implementadas y ejemplo verificable

- `edad_inicial`: vueltas completas de uso antes del stint, entero >= 0.
- En la vuelta local j, `a_j = edad_inicial + j`, con j entre 1 y n.
- La edad corresponde al final de la vuelta. Un juego nuevo usa 1, 2, 3...
- Quitar una vuelta del ajuste no reduce la edad de las posteriores.
- Parar al final de la vuelta k deja k vueltas en el primer tramo y comienza
  el siguiente en k+1. Dos paradas k1, k2 dan longitudes k1, k2-k1, N-k2.
- El tiempo de tramo es `n*b + d*(n*a0 + n*(n+1)/2)`, en segundos.
- El costo adicional por boxes se agregará una vez por parada a las vueltas
  normales. El núcleo actual calcula solamente vueltas normales.

Ejemplo SINTÉTICO, calculable a mano: b=90 s, d=0.1 s por vuelta de edad,
edad inicial 0 y tres vueltas: 90.1 + 90.2 + 90.3 = 270.6 s.
Un juego con edad inicial 4 y dos vueltas: 90.5 + 90.6 = 181.1 s.

## Orden de implementación propuesto

1. **Hecho:** edades y tiempos de vuelta/tramo, ejemplos JSON compartibles con R.
2. Evaluar una estrategia suministrada: longitudes, juegos individuales, edades,
   suma de pérdidas por boxes, límites, extrapolación y comprobación de entradas.
3. Enumerar una y dos paradas en vueltas enteras, sin porcentajes obligatorios;
   verificar manualmente la mejor opción en una carrera sintética corta.
4. Auditar una práctica real de un piloto antes de construir un catálogo completo:
   descarga local reproducible, normalización, edades, limpieza y trazabilidad.
5. Ajustar pendientes, configurar/comprobar bases comparables y emitir diagnósticos.
6. Integrar catálogo y selección por consola, confirmación de parámetros y salida
   de hasta tres estrategias. Evaluar científicamente en casos reservados.
7. Reconstruir neutralizaciones y desarrollar respuestas condicionales más adelante.

Funciones previstas (se crearán cuando se alcance cada etapa):

| Responsabilidad | Funciones | Entradas y salidas principales |
| --- | --- | --- |
| Fórmulas actuales | edades_stint, tiempo_vuelta, tiempo_stint | Parámetros explícitos -> edades o segundos |
| Estrategias | evaluar_estrategia, buscar_estrategias | Modelos, juegos, reglas, boxes -> alternativas y advertencias |
| Fuentes | descargar_sesion, normalizar_vueltas | Identificador y fuente -> datos locales y registros normalizados |
| Preparación | clasificar_tramo_seco, limpiar_vueltas | Registros -> elegibilidad, exclusiones y candidatos a revisión |
| Estimación | ajustar_stints, construir_modelos, diagnosticar_modelos | Vueltas y configuración -> parámetros, procedencia y diagnósticos |

Mantener pocas piezas: estimadorF1.py para fórmulas y después búsqueda;
datos.py cuando exista descarga; ajuste.py cuando exista estimación.
La consola orquestará funciones sin contener las fórmulas. No hace falta
crear ahora paquetes, interfaz gráfica ni clases de servicios.

## Entradas y políticas que faltan cerrar

- Caso piloto inicial: temporada, GP y piloto. Verificar reglamento específico
  y restricciones de la competencia antes de emitir estrategias admisibles.
- Distancia programada conocida antes de la largada: no usar la distancia
  finalmente recorrida como entrada retrospectiva si difiere de la prevista.
- Inventario por juego individual (identificador, compuesto, edad y disponibilidad).
  Un stint registrado no demuestra que ese juego esté disponible para la carrera.
- Pérdida adicional bajo verde, en segundos, con fuente, cálculo o supuesto.
- Límites por compuesto y extrapolación: propuesta de advertir siempre fuera
  del rango observado y exigir un límite de escenario explícito para permitirlo.
  El límite no equivale a vida máxima demostrada.
- Calidad operativa: definir mínimos configurables de vueltas y rango de edades
  tras inspeccionar el caso piloto; no presentar umbrales como validados.
- Compuesto inicial opcional. No suponer juegos nuevos ilimitados.
- Sesiones: conservar solo el inicio seco utilizable si después llueve; descartar
  sesiones que comienzan mojadas y se secan. Si no puede establecerse el estado
  inicial, marcarlo indeterminado y excluirlo hasta revisar evidencia.

## Datos, cobertura y reproducibilidad

Separar registros originales, variables calculadas y supuestos. Guardar respuestas
crudas por fuente/sesión con fecha de descarga, versión y consultas. Mantener
valores ausentes como ausentes y no concatenar duplicados de dos fuentes.
Aplicar un corte de información previo a la largada: incluir solo prácticas
terminadas y antecedentes previos. Descargar hoy no demuestra que un dato o su
revisión estuviese disponible entonces; registrar esa limitación retrospectiva.

Documentación oficial de los proyectos consultada:

- [OpenF1](https://openf1.org/docs/): cobertura histórica desde 2023; revisar
  disponibilidad efectiva por sesión. En pit, lane_duration es tiempo en pit lane
  y stop_duration tiempo detenido; ninguno es directamente la pérdida relativa.
  El documento académico menciona lane_stop, que no figura en la documentación.
  rainfall indica precipitación, no certifica superficie seca.
- [FastF1, código y documentación de campos](https://github.com/theOehrly/Fast-F1/blob/master/fastf1/core.py):
  existen LapTime, Compound, TyreLife, PitInTime, PitOutTime y TrackStatus.
  Su implementación incrementa TyreLife en cada vuelta del stint. Verificar
  adaptación y cobertura antes de usarlo; no asumir coincidencia perfecta con
  OpenF1. El sitio docs.fastf1.dev devolvió HTTP 403 durante esta consulta.

No se descargaron observaciones de carreras ni se implementó todavía ningún
adaptador. Para estudiar sesiones anteriores a 2023, verificar primero la
cobertura efectiva de FastF1, sin prometer todos los campos desde 2018.

## Pruebas de funcionamiento y evaluación científica

Ejecutar `python3 -m unittest -v` desde esta carpeta. No requiere dependencias.
casos_sinteticos.json contiene entradas y resultados esperados calculados a mano;
R deberá reproducirlos con tolerancia absoluta de 1e-9 segundos. Las validaciones
de este núcleo aceptan números escalares estándar y enteros Python; los
adaptadores convertirán y validarán tipos externos explícitamente.

Estas pruebas no demuestran precisión predictiva ni capacidad de elegir una
estrategia real. No se ha probado todavía la búsqueda porque no está implementada.

Para evaluación científica: separar por stint, preferir validación dejando uno
completo fuera cuando haya datos suficientes y reportar MAE en segundos, rangos
observados, consistencia y extrapolación. No ajustar un offset con el stint de
prueba y luego presentar su error como predicción independiente. El error
absoluto de una práctica reservada también refleja diferencias de combustible.
Si no se puede evaluar un compuesto, indicarlo, sin fabricar una métrica.

Las carreras objetivo se reservan para evaluación y nunca ajustan su propia
recomendación. Una comparación con lo ocurrido no revela el resultado de una
estrategia alternativa no ejecutada. P/F/R y E requieren revisar las ecuaciones
embebidas y fijar un protocolo; D corresponde a la futura adaptación VSC/SC.
La R de rapidez del paper debe distinguirse de correlación r y de R² del ajuste.
Una abstención por datos insuficientes debe registrarse y distinguirse de un
fallo del programa y de una estrategia inválida al informar cobertura y fiabilidad.
