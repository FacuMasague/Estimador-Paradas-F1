# VSC y SC: ejecución del plan y resultados

Notas para el paper (10/11/2026). Plan de referencia: pestaña "TP3 2da Entrega" del
documento, con el ajuste acordado: probabilidad por circuito, momento en 5 tramos y
decisión de parar o seguir resumida en 10 secciones. El programa usa solo OpenF1;
FastF1 (2019–2026) se usó únicamente para elegir cuánto pesa el circuito.

## 1. Episodios (`episodios.R`, `episodiosNeutralizacion()`)

Reconstruidos desde los mensajes de dirección de carrera, contando episodios y no
mensajes: VSC de "DEPLOYED" a "ENDING"; SC de "DEPLOYED" a "IN THIS LAP"; un VSC seguido
de un SC sin cierre es una conversión; bandera roja o a cuadros cierran el episodio.

- 87 carreras (70 secas), 113 episodios: 59 SC y 54 VSC.
- Cierre: 97 con su mensaje de fin, 4 conversiones de VSC a SC, 6 por bandera roja y 5
  por fin de carrera. Ninguno sin cierre.
- Duración mediana: SC 4 vueltas (unos 7 minutos), VSC 2 vueltas (unos 2 minutos).

## 2. Probabilidad por circuito (`calibrarProbabilidades.R`, `calibrarProbabilidadesFF1.R`)

Cada carrera seca se pronosticó solo con carreras anteriores.

- Carreras secas de OpenF1 (70): 47 % con SC, 44 % con VSC, 69 % con alguno.
  Escenarios: ninguno 22, solo VSC 15, solo SC 17, ambos 16.
- **Hallazgo:** con 3 o 4 ediciones por circuito, la frecuencia del circuito pronostica
  peor que la tasa general (Brier SC 0,277 con el mejor ajuste por circuito contra 0,259
  con la tasa general).
- Con FastF1 2019–2026 (128 carreras secas, hasta 8 ediciones por circuito; sin EE. UU.,
  México y Brasil 2026): las diferencias entre circuitos no son significativas
  (chi² p = 0,13 SC; p = 0,77 VSC). El mejor pronóstico de SC acerca la frecuencia del
  circuito a la general con k = 8 carreras ficticias (Brier 0,251 contra 0,252 de la
  general); para VSC lo mejor es la tasa general.
- **Decisión:** p(circuito) = (episodios del circuito + k · tasa general) / (ediciones + k),
  con k = 8 para SC y tasa general para VSC. Se informa siempre cuántas ediciones hay.

## 3. Momento de inicio, 5 tramos (`momentoNeutralizacion()`)

Con todas las carreras juntas. FastF1, carreras secas: SC 37 / 14 / 10 / 15 / 11 y VSC
19 / 13 / 14 / 19 / 9 por tramo de 20 %. El SC se concentra en el primer 20 % (largada);
el VSC está más repartido.

## 4. Pérdida en boxes bajo VSC y SC (`perdidaNeutralizada.R`)

Medida igual en los tres casos: vuelta de entrada + vuelta de salida del que para menos la
mediana de las mismas vueltas de los autos que siguieron en pista en ese momento.

| | Paradas | Pérdida mediana | Proporción de la pérdida verde (misma carrera) |
|---|---|---|---|
| Verde | 1.532 | 22,5 s | 1 |
| VSC | 93 | 14,1 s | 0,57 (20 carreras; cuartiles 0,52 a 0,72) |
| SC | 138 | 14,4 s | 0,50 (24 carreras; cuartiles 0,41 a 0,79) |

No incluye la pérdida o ganancia de posiciones por el agrupamiento bajo SC.

## 5. Decisión, ventanas y medida D

- `decisionNeutralizacion()`: ante un episodio que empieza en la vuelta L compara parar
  ahora (pérdida verde × proporción) contra seguir con el mejor plan desde ese estado
  (compuesto, edad, paradas hechas, compuestos usados), con las mismas restricciones del
  modelo (2 paradas como máximo, dos compuestos, stint mínimo, vida máxima).
- `ventanasNeutralizacion()`: vueltas consecutivas con la misma respuesta y resumen en 10
  secciones.
- `medidaD()`: ahorro esperado de adaptarse frente al plan fijo, ponderado por la
  probabilidad del circuito y el momento; si una parada del plan fijo cae dentro del
  episodio, esa parada también se abarata.

Supuestos: el tiempo bajo neutralización es igual para ambas opciones; no se modelan la
menor degradación bajo neutralización, las posiciones ni el tráfico; solo el primer
episodio de cada tipo.

## 6. Validación contra decisiones reales (`validarNeutralizaciones.R`, `verValidacion.R`)

En las carreras secas con VSC/SC y prácticas utilizables (21 GP, 34 episodios), para cada
piloto con modelo y su estado real al empezar el episodio (compuesto, edad, paradas,
compuestos usados), se comparó la recomendación con si entró a boxes durante el episodio.
374 decisiones; 281 con la opción de parar todavía disponible.

Con las proporciones medidas directamente (VSC 0,57; SC 0,50):

| | Decisiones | Pararon | Programa dice parar | Acuerdo | Regla fija más común |
|---|---|---|---|---|---|
| SC | 141 | 46 % | 36 % | 70 % | 54 % ("seguir") |
| VSC | 140 | 19 % | 51 % | 64 % | 81 % ("seguir") |

- Bajo SC la relación es clara: con ahorro estimado mayor a 3 s paró el 80–87 %; con
  pérdida mayor a 3 s, el 10–12 %.
- Bajo VSC el programa recomendaba parar mucho más que los equipos.
- Compuesto siguiente: coincide en 46 de 61 paradas (75 %).

**Problema del plan:** medir la pérdida solo en las paradas hechas sesga la medición,
porque los equipos paran bajo neutralización cuando el momento les conviene. La pérdida
típica para un momento cualquiera es mayor en VSC, y en SC el agrupamiento del pelotón
hace que parar valga más de lo que dice el tiempo de vuelta.

## 7. Calibración con decisiones reales (`calibrarFactores.R`)

Se eligió la proporción que hace que "parar si el ahorro es positivo" coincida con lo
que hicieron los pilotos, validando con cada GP afuera.

| | Proporción | Acuerdo | Acuerdo validado por GP | Regla fija |
|---|---|---|---|---|
| SC | 0,32 (0,28 a 0,37 dan el mismo acuerdo) | 82 % | 82 % | 54 % |
| VSC | ninguna supera a la regla fija (mejor > 1) | 81 % | 70 % | 81 % ("nunca parar") |

Decisión: SC 0,32; VSC 1,0 (la parada cuesta lo mismo que en verde; acuerdo 79 %). En la
práctica un VSC de unas 2 vueltas no abarata la parada lo suficiente: puede terminar
mientras el auto está en boxes.

## 8. Ejecución en los 200 casos de investigación (`investigacion_casos.csv`)

Con las proporciones calibradas (SC 0,32; VSC 1,0):

- **F (fiabilidad):** 175 de 200 casos (87,5 %) devuelven estrategia y respuestas
  condicionales. Los 25 restantes son de Miami, España y Hungría 2023: OpenF1 empieza en
  2023 y esas carreras tienen menos de 10 carreras secas anteriores para estimar
  probabilidades.
- **R (rapidez):** 35 s por caso (mediana; antes, sin neutralizaciones, unos 19 s).
- **P (precisión):** sin cambios; las neutralizaciones no modifican la estrategia base.
- **Probabilidades:** SC mediana 0,50 (cuartiles 0,40–0,50) y VSC 0,40; los circuitos
  tenían 0 a 2 ediciones anteriores.
- **Ventanas:** ante un SC conviene parar en 4 de las 10 secciones (mediana; entre 1 y 8
  según el caso), casi siempre en la primera mitad, antes de la parada del plan. Ante un
  VSC nunca conviene parar fuera de la parada ya prevista.
- **Medida D:** 1,5 s por carrera (cuartiles 1,2–1,8; máximo 4,7); si hay SC, 3,4 s
  (cuartiles 2,6–4,0). Zandvoort 2024 es el caso con más beneficio (9,5 s si hay SC).

## 9. Veredicto sobre el plan

| Parte del plan | Resultado |
|---|---|
| Reconstruir episodios de VSC/SC | Funciona: 113 episodios, todos cerrados |
| Probabilidad por circuito | **No se sostiene tal como estaba**: con 3–8 ediciones no hay diferencias significativas entre circuitos. Se reemplazó por la tasa general ajustada levemente por circuito |
| Momento en 5 tramos | Funciona con todas las carreras juntas; por circuito no hay datos |
| Pérdida en boxes diferenciada | **Medirla en las paradas hechas está sesgado**. Se corrigió calibrando con decisiones reales: SC 0,32 de la verde; bajo VSC no hay regla mejor que no parar |
| Decisión parar o seguir y ventanas en 10 secciones | Funciona para SC (82 % de acuerdo con los equipos, contra 54 % de una regla fija); para VSC no supera a "nunca parar" |
| Medida D | Positiva pero chica: 1,5 s por carrera, 3,4 s si hay SC; mucho menor que la incertidumbre del modelo para elegir estrategias (margen de empate de unos 59 s) |

Conclusión para el paper: la parte de neutralizaciones del plan es ejecutable y la
recomendación ante un SC reproduce bien lo que hacen los equipos. Dos supuestos del plan
no se cumplieron con los datos: que cada circuito tiene una probabilidad propia
estimable y que el VSC abarata la parada de forma aprovechable. El beneficio esperado de
adaptarse (D) es real dentro del simulador pero pequeño frente a la incertidumbre del
modelo.

Limitaciones: no se modelan posiciones ni agrupamiento más allá de la proporción
calibrada, ni la menor degradación bajo neutralización; la proporción del SC se calibró
con las mismas carreras usadas para validarla (validación dejando fuera cada GP); hay
pocas ediciones por circuito en OpenF1.

## 10. Decisión final: solo SC (10/11/2026)

Facundo decidió dejar el VSC fuera de los resultados: su beneficio depende del momento
exacto en que empieza y de dónde está el auto, que no se conocen de antemano. El programa
analiza solo SC (`parametros$neutralizacion$tipos = "SC"`).

### Validación contra todos los SC reales medibles

Medibles: carreras secas con prácticas utilizables (fines de semana de tres prácticas),
episodios cerrados con su mensaje y al menos 3 vueltas antes del final, pilotos con modelo
completo que terminaron la carrera y todavía podían parar. 16 episodios en 14 GP, 178
pilotos-episodio, 141 con la opción de parar disponible.

| | Decisiones | Pararon | Programa: parar | Acuerdo | "Nunca parar" |
|---|---|---|---|---|---|
| SC (proporción 0,32) | 141 | 46 % | 54 % | **82 %** | 54 % |

La proporción 0,32 se calibró con estas mismas decisiones; dejando fuera cada GP para
calibrar, el acuerdo también es 82 %.

- Con ahorro estimado mayor a 0 s paró el 65–86 % de los pilotos; con pérdida mayor a 3 s,
  menos del 10 % (gráfico 2).
- Compuesto siguiente cuando ambos paran: coincide en 28 de 58 (48 %). El programa elige
  a veces blando o medio donde los equipos montan duro.
- Por episodio (`validacion_sc_por_episodio.csv`):

| SC | Pilotos | Pararon | Programa: parar | Acuerdo |
|---|---|---|---|---|
| Gran Bretaña 2023, v33 | 5 | 1 | 5 | 20 % |
| Canadá 2023, v12 | 5 | 0 | 0 | 100 % |
| Japón 2023, v1 | 9 | 2 | 0 | 78 % |
| Arabia Saudita 2023, v18 | 13 | 7 | 10 | 77 % |
| Singapur 2023, v20 | 11 | 9 | 11 | 82 % |
| Arabia Saudita 2024, v7 | 12 | 9 | 10 | 58 % |
| Austria 2025, v1 | 12 | 1 | 0 | 92 % |
| Baréin 2025, v32 | 6 | 6 | 6 | 100 % |
| Zandvoort 2025, v23 | 10 | 8 | 10 | 80 % |
| Zandvoort 2025, v53 | 8 | 7 | 8 | 88 % |
| Zandvoort 2025, v65 | 3 | 0 | 1 | 67 % |
| Imola 2025, v46 | 9 | 7 | 9 | 78 % |
| Arabia Saudita 2025, v1 | 12 | 1 | 0 | 92 % |
| Bélgica 2026, v1 | 13 | 1 | 0 | 92 % |
| Japón 2026, v22 | 13 | 6 | 6 | 100 % |

El peor caso es Gran Bretaña 2023 (vuelta 33): el programa manda a parar a los 5 pilotos
medidos y solo paró 1. Los SC de la primera vuelta se resuelven bien (nadie para con
neumáticos nuevos).

### Gráficos (`graficosSC.R`, carpeta `graficos_sc/`)

1. `1_cada_sc.png`: cada SC y cada piloto, con el ahorro estimado, si paró y si el
   programa coincidió.
2. `2_ahorro_vs_decision.png`: proporción de pilotos que paró según el ahorro estimado.
3. `3_por_episodio.png`: pilotos que pararon y que el programa manda a parar, por SC.
4. `4_por_momento.png`: acuerdo según el momento de la carrera y matriz de aciertos.

Además, el gráfico de cada caso de investigación tiene un 4.º panel con el ahorro de
parar si un SC empieza en cada vuelta.

### 200 casos con solo SC

Sin el VSC los resultados del punto 8 no cambian (el VSC ya aportaba 0 a D): F 175 de 200,
D 1,5 s por carrera (cuartiles 1,2–1,8; máximo 4,7) y 3,4 s si hay SC; parar conviene en
4 de 10 secciones (mediana). R baja a 29 s por caso.
