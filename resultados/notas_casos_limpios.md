# Casos de carrera limpia: diferencias entre la estrategia estimada y la real

Notas para el paper (10/11/2026). Versión del modelo: calibrado con los 54 GP de tres
prácticas (casos incluidos), corrección de nivel −1,12 % y vida máxima por compuesto.
Datos: `casos_limpios.csv`, `trafico.csv`, `contextoLimpios.R`.

## Criterio de selección (fijado antes de mirar las predicciones)

- Al menos 75 % de las vueltas limpias con 2 s o más al auto de adelante en pista.
- Como máximo 1 cambio de posición en pista (sin contar boxes).
- Sin ser doblado.

Resultado: 10 casos en 8 GP. Ninguno tuvo penalizaciones, investigaciones ni paradas
lentas (mensajes de dirección de carrera y tiempos de boxes de OpenF1). Los 10 terminaron
1.º o 2.º y estuvieron entre los 4 mejores ritmos de su carrera.

## Estrategia estimada frente a la real

En el modelo el orden de los stints no cambia el tiempo: M-H parando al 30 % equivale a
H-M parando al 70 %. Por eso se comparan los compuestos usados y las vueltas de cada uno.

| Caso | Real | Modelo | Clasificación | Explicación |
|---|---|---|---|---|
| Verstappen, Zandvoort 2024 | M 27 + H 45 | H 37 + M 35 | Coincide (otro orden) | Mismos compuestos y paradas; reparte las vueltas de forma más pareja. |
| Verstappen, Abu Dabi 2025 | M 23 + H 35 | H 34 + M 24 | Coincide | Difiere en 1 vuelta. |
| Piastri, Hungría 2024 | M-H-M (18+29+23) | H-H-M (23+24+23) | Razón documentada | Mismos compuestos y paradas. McLaren hizo parar antes a Norris para cubrir a Hamilton y luego dio órdenes de equipo; las paradas respondieron a la pelea, no al neumático. [Motorsport.com](https://www.motorsport.com/f1/news/f1-hungarian-gp-race-report/10637230/), [Autosport](https://www.autosport.com/f1/news/f1-hungarian-gp-race-report/10637228/) |
| Piastri, Abu Dabi 2025 | H 41 + M 17 | H 34 + M 24 | Razón documentada | Mismos compuestos. Definición del campeonato: McLaren dividió estrategias y le dio a Piastri un primer stint largo de duro para neutralizar a Verstappen. [PlanetF1](https://www.planetf1.com/news/mclaren-andrea-stella-explains-tactics-neutured-max-verstappen-interference-2025-abu-dhabi-grand-prix), [Autosport](https://www.autosport.com/f1/news/why-mclarens-tactic-to-kill-the-drama-in-the-2025-f1-title-decider-was-so-effective/10783265/) |
| Piastri, Monza 2024 | M-H-H (16+22+15) | H 27 + S 26 | Factor externo | El desgranado obligó a McLaren a 2 paradas; Leclerc ganó con 1 parada, que analistas consideran apenas más rápida pero riesgosa. [Autosport](https://autosport.com/f1/news/f1-italian-gp-leclerc-one-stop-strategy-defeats-mclarens/10650239), [Autosport, análisis](https://www.autosport.com/f1/news/the-50-cent-coin-disaster-risk-that-kept-mclaren-off-the-best-monza-strategy/10650508/), [Motor Sport](https://www.motorsportmagazine.com/archive/article/november-2024/36/how-mclaren-was-outwitted-by-a-wily-ferrari-f1-team-at-monza) |
| Russell, Las Vegas 2024 | M-H-H (12+20+18) | M 21 + S 29 | Factor externo | Se esperaba 1 parada; el desgranado del medio y del duro llevó a casi todos a 2. [Racer](https://racer.com/2024/11/24/russell-dominates-in-las-vegas-as-verstappen-clinches-fourth-title/), [The Race](https://www.the-race.com/formula-1/mark-hughes-extreme-factors-behind-mercedes-vegas-domination/) |
| Verstappen, Baréin 2024 | S-H-S (17+20+20) | H 30 + M 27 | Factor externo | Carrera de alta degradación donde se esperaban 2 paradas; Red Bull reservó juegos de blando extra mientras el resto guardó duros, y un análisis previo señalaba blando-duro-blando como lo más rápido. El modelo no conoce los juegos disponibles. [PlanetF1](https://www.planetf1.com/news/max-verstappen-red-bull-severe-disadvantage-bahrain), [Pirelli vía Pitpass](https://www.pitpass.com/77108/Bahrain-Grand-Prix-Race-team-notes-Pirelli). La secuencia exacta sale de OpenF1; no se confirmó en fuentes. |
| Norris, Singapur 2024 | M 30 + H 32 | H 39 + S 23 | Error del modelo | Sin razón particular encontrada. Sesgo del modelo hacia el blando. |
| Verstappen, Singapur 2024 | M 29 + H 33 | H 39 + S 23 | Error del modelo | Igual que Norris. |
| Verstappen, España 2023 | M-H-S (26+26+14) | H 35 + M 31 | Error del modelo | Sin nada raro en los datos; no se investigó en fuentes. La real usó 2 paradas con los tres compuestos. |

Resumen: 2 coinciden, 2 tienen una razón de equipo documentada, 3 un factor externo
(desgranado o juegos disponibles) y 3 son error del modelo. Mismas paradas: 6/10.

Para el paper: el desgranado y los juegos disponibles no son excusas del caso sino
limitaciones del modelo, que no los ve con datos de prácticas.

## Diferencia de tiempo (para la sección de limitaciones)

El modelo es más lento que la carrera real en los 10 casos (mediana 65 s, 1,3 %). No se
debe a incidentes: la corrección de nivel (−1,12 %) es un promedio del campo y el error
depende del ritmo (874 piloto-GP, `error_nivel3.csv`):

| Pilotos según ritmo de carrera | Error sin corregir | Error tras −1,12 % |
|---|---|---|
| 15 % más rápido | −2,04 % | −0,92 % (≈ −0,78 s/vuelta) |
| 15–30 % | −1,39 % | −0,27 % |
| 30–50 % | −1,01 % | +0,11 % |
| Resto | −0,70 % | +0,42 % |

Los casos limpios son casi todos punteros, así que heredan ese sesgo (unos 45–55 s en
60–70 vueltas). Interpretación probable: los equipos de punta esconden más ritmo en
prácticas. No se puede corregir antes de la carrera porque el ritmo de carrera no se
conoce. Caso aparte: en Zandvoort 2024 Verstappen perdió en boxes 5 s menos de lo
estimado (24,0 s contra 29,1 s).
