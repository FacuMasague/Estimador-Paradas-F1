# Pruebas de funcionamiento con datos SINTETICOS, no observaciones de F1.
# Ejecutar desde la carpeta del proyecto: Rscript test_practicas.R
library(DBI)
library(RSQLite)

# Cargar solamente las definiciones, sin ejecutar catalogos, menus ni consultas.
funciones <- new.env(parent = globalenv())
sys.source("modeloEstrategias.R", envir = funciones)
for (expresion in parse("estimadorR.R")) {
  if (is.call(expresion) && identical(expresion[[1]], as.name("<-")) &&
      is.call(expresion[[3]]) && identical(expresion[[3]][[1]], as.name("function"))) {
    eval(expresion, envir = funciones)
  }
}

inicio <- as.POSIXct("2023-01-01 10:00:00", tz = "UTC")
combPrueba <- list(sensibilidad = 0.03, consumo = 1.8, factorQualy = 1.25,
                   maxVueltasQualy = 6, margenVueltas = 1)
vueltas <- data.frame(
  session_key = rep(1L, 7), driver_number = rep(1L, 7),
  lap_number = 1:7,
  date_start = c("2023-01-01T10:00:00+00:00", "2023-01-01T10:01:00+00:00",
                 "2023-01-01T10:02:00+00:00", "2023-01-01T10:03:00+00:00",
                 "2023-01-01T10:04:00+00:00", NA, "2023-01-01T10:00:00+00:00"),
  lap_duration = c(60, 60, 60, 60, 61, 60, NA),
  is_pit_out_lap = c(1L, rep(0L, 6))
)
stints <- data.frame(
  session_key = c(1L, 1L, 2L), driver_number = c(1L, 1L, 1L),
  stint_number = c(1L, 2L, 1L), compound = c("MEDIUM", "SOFT", "HARD"),
  lap_start = c(1L, 3L, 1L), lap_end = c(3L, 7L, 7L),
  tyre_age_at_start = c(0L, 5L, 0L)
)
r <- funciones$anotarVueltasPiloto(vueltas, stints, inicio, inicio + 300)
stopifnot(
  nrow(r) == nrow(vueltas),
  identical(r$lap_number, vueltas$lap_number),
  identical(r$is_pit_out_lap, vueltas$is_pit_out_lap),
  all(r$estado_horario[1:4] == "dentro_del_tramo"),
  r$estado_horario[5] == "fuera_del_tramo",
  all(r$estado_horario[6:7] == "fecha_o_duracion_invalida"),
  r$estado_stint[3] == "ambiguo", is.na(r$compound[3]),
  r$compound[1] == "MEDIUM", r$compound[4] == "SOFT",
  r$tyre_age_at_start[4] == 5
)
# Un final exactamente en el limite se conserva; uno posterior se rechaza.
v <- vueltas[1, ]; v$lap_duration <- 300
stopifnot(funciones$anotarVueltasPiloto(v, stints, inicio, inicio + 300)$estado_horario == "dentro_del_tramo")
v$lap_duration <- 300.001
stopifnot(funciones$anotarVueltasPiloto(v, stints, inicio, inicio + 300)$estado_horario == "fuera_del_tramo")
# Un mismo numero en dos sesiones no es un duplicado; en la misma sesion, si.
v <- rbind(vueltas[1, ], vueltas[1, ])
v$session_key[2] <- 2L
stopifnot(!any(funciones$anotarVueltasPiloto(v, stints, inicio, inicio + 300)$numero_repetido))
v$session_key[2] <- 1L
stopifnot(all(funciones$anotarVueltasPiloto(v, stints, inicio, inicio + 300)$numero_repetido))
# Los limites desconocidos no permiten una asignacion unica demostrable.
s <- stints; s$lap_end[1] <- NA_integer_
stopifnot(all(funciones$anotarVueltasPiloto(vueltas, s, inicio, inicio + 300)$estado_stint == "datos_stint_invalidos"))
stopifnot(all(funciones$anotarVueltasPiloto(vueltas, stints[FALSE, ], inicio, inicio + 300)$estado_stint == "sin_coincidencia"))
stopifnot(nrow(funciones$anotarVueltasPiloto(vueltas[FALSE, ], stints, inicio, inicio + 300)) == 0)

# Base temporal sintetica: tres practicas, varios pilotos y una descarga fallida.
probarBaseTemporal <- function() {
  ruta <- tempfile(fileext = ".sqlite3")
  on.exit(unlink(ruta), add = TRUE)
  con <- dbConnect(SQLite(), ruta)
  on.exit(if (dbIsValid(con)) dbDisconnect(con), add = TRUE)
  v <- vueltas[1:2, ]
  v$registro <- 1:2
  v2 <- v; v2$session_key <- 2L
  v3 <- v; v3$session_key <- 3L
  otro <- v; otro$driver_number <- 2L
  dbWriteTable(con, "vueltas", rbind(v, v2, v3, otro))
  s <- stints[1, ]; s$registro <- 1L
  s2 <- s; s2$session_key <- 2L
  s3 <- s; s3$session_key <- 3L
  dbWriteTable(con, "stints", rbind(s, s2, s3))
  # Control de carrera: solo el inicio de cada sesion. La sesion 3 no tiene
  # descarga de race_control, para comprobar la marca control_no_verificable.
  dbWriteTable(con, "control_carrera", data.frame(
    session_key = 1:3, registro = 1L, date = "2023-01-01T09:59:00+00:00",
    category = "SessionStatus", flag = NA_character_, scope = NA_character_,
    sector = NA_integer_, message = "SESSION STARTED"
  ))
  d <- data.frame(
    endpoint = c(rep(c("laps", "stints", "race_control"), 2), "laps", "stints"),
    estado = c("ok", "ok", "ok", "error", "ok", "ok", "ok", "ok"),
    filas = c(4L, 1L, 1L, 2L, 1L, 1L, 2L, 1L),
    parametros_json = sprintf('{"session_key":%d}', c(1, 1, 1, 2, 2, 2, 3, 3))
  )
  dbWriteTable(con, "descargas", d)
  dbDisconnect(con)
  seleccion <- list(granPremio = data.frame(meeting_key = 1L),
                    piloto = data.frame(driver_number = 1L))
  clima <- data.frame(meeting_key = rep(1L, 3), session_key = 1:3,
                      nombre = paste("Practice", 1:3),
                      estado = rep("candidato_completo", 3), motivo = rep("prueba_sintetica", 3),
                      inicio_candidato = rep(inicio, 3), fin_candidato = rep(inicio + 300, 3))
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta, combPrueba)
  stopifnot(nrow(r$resumen) == 3,
            r$resumen$estado_datos[2] == "descargas_ausentes_o_no_ok",
            r$resumen$estado_datos[3] == "revisado",
            all(r$vueltas$driver_number == 1),
            setequal(r$vueltas$session_key, c(1L, 3L)),
            r$resumen$vueltas_registradas[1] == 2,
            isTRUE(r$resumen$control_verificado[1]),
            identical(r$resumen$control_verificado[3], FALSE),
            all(r$vueltas$control_no_verificable[r$vueltas$session_key == 3]),
            !any(r$vueltas$control_no_verificable[r$vueltas$session_key == 1]),
            "apta_ajuste" %in% names(r$vueltas))
  # Descartar una practica por clima no impide revisar las otras.
  clima$estado[3] <- "descartar_inicio_con_lluvia"
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta, combPrueba)
  stopifnot(r$resumen$estado_datos[3] == "no_revisado_por_clima",
            all(r$vueltas$session_key == 1))
  # Si no hay ningun tramo candidato, conservar un resumen de tres filas y tablas vacias.
  clima$estado[] <- "indeterminado"
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta, combPrueba)
  stopifnot(nrow(r$resumen) == 3, nrow(r$vueltas) == 0, nrow(r$stints) == 0)
  # Detectar cantidades inconsistentes sin detener las demas sesiones.
  clima$estado[] <- "candidato_completo"
  con <- dbConnect(SQLite(), ruta)
  dbExecute(con,
    "UPDATE descargas SET filas=999 WHERE endpoint='laps' AND json_extract(parametros_json, '$.session_key')=1")
  dbDisconnect(con)
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta, combPrueba)
  stopifnot(r$resumen$estado_datos[1] == "cantidad_de_filas_inconsistente",
            r$resumen$estado_datos[3] == "revisado")
  # El piloto puede no tener registros aunque la descarga de la sesion sea correcta.
  seleccion$piloto$driver_number <- 99L
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta, combPrueba)
  stopifnot(r$resumen$estado_datos[3] == "sin_vueltas_del_piloto", nrow(r$vueltas) == 0)
}

probarBaseTemporal()

# Edad al inicio de la vuelta: tyre_age_at_start + vueltas desde el inicio del stint.
r <- funciones$anotarVueltasPiloto(vueltas, stints, inicio, inicio + 300)
stopifnot(r$edad_inicio[1] == 0, r$edad_inicio[2] == 1,
          is.na(r$edad_inicio[3]),             # stint ambiguo: sin edad
          r$edad_inicio[4] == 5 + (4 - 3))

# --- Intervalos de control de carrera (mensajes SINTETICOS) ---
mensajes <- function(segundos, flag, scope, sector, message) {
  data.frame(date = format(inicio + segundos, "%Y-%m-%dT%H:%M:%OS3+00:00"),
             flag = flag, scope = scope, sector = sector, message = message,
             category = "Flag")
}
control <- rbind(
  mensajes(10, "YELLOW", "Sector", 5L, "YELLOW IN TRACK SECTOR 5"),
  mensajes(11, "CLEAR", "Sector", 7L, "CLEAR IN TRACK SECTOR 7"),   # otro sector: no cierra
  mensajes(12, "CLEAR", "Sector", 5L, "CLEAR IN TRACK SECTOR 5"),
  mensajes(100, "RED", "Track", NA, "RED FLAG"),
  mensajes(150, "GREEN", "Track", NA, "GREEN LIGHT - PIT EXIT OPEN"),
  mensajes(200, NA, NA, NA, "VIRTUAL SAFETY CAR DEPLOYED"),
  mensajes(230, NA, NA, NA, "VIRTUAL SAFETY CAR ENDING"),
  mensajes(250, "DOUBLE YELLOW", "Sector", 2L, "DOUBLE YELLOW IN TRACK SECTOR 2")
)
iv <- funciones$armarIntervalosControl(control, inicio + 300)
stopifnot(
  identical(iv$tipo, c("amarilla", "bandera_roja", "vsc_sc", "amarilla")),
  as.numeric(iv$hasta[1] - inicio, units = "secs") == 12,
  as.numeric(iv$hasta[2] - inicio, units = "secs") == 150,
  as.numeric(iv$hasta[3] - inicio, units = "secs") == 230,
  iv$hasta[4] == inicio + 300, !iv$cerrado[4],  # sin cierre: hasta el fin del tramo
  nrow(funciones$armarIntervalosControl(control[FALSE, ], inicio + 300)) == 0
)

# --- Marcas identificables ---
v <- data.frame(
  session_key = 1L, driver_number = 1L, lap_number = 1:6,
  date_start = format(inicio + c(0, 20, 60, 140, 180, 240), "%Y-%m-%dT%H:%M:%OS+00:00"),
  lap_duration = c(15, 30, 30, 30, 30, 30),
  is_pit_out_lap = c(1L, 0L, 0L, 0L, 1L, NA)
)
s <- data.frame(session_key = 1L, driver_number = 1L, stint_number = 1L,
                compound = "SOFT", lap_start = 1L, lap_end = 6L, tyre_age_at_start = 0L)
v <- funciones$anotarVueltasPiloto(v, s, inicio, inicio + 300)
m <- funciones$marcarVueltasNoRepresentativas(v, iv, TRUE)
stopifnot(
  nrow(m) == 6,
  m$motivos_exclusion[1] == "salida_boxes",
  m$con_amarilla[1], m$candidata_ajuste[1] == FALSE,     # 0-15 s toca la amarilla 10-12 s
  m$con_amarilla[2] == FALSE, m$candidata_ajuste[2],     # la amarilla sola no excluye
  m$motivos_exclusion[3] == "", m$candidata_ajuste[3]    # 60-90 s no toca la roja 100-150 s
)
stopifnot(
  m$motivos_exclusion[4] == "entrada_boxes_inferida;bandera_roja", # 140-170 s, la 5 es de salida
  m$motivos_exclusion[5] == "salida_boxes;vsc_sc",                  # 180-210 s
  m$motivos_exclusion[6] == "salida_boxes_desconocida",             # 240-270 s
  m$con_amarilla[6]
)
# Sin control verificable todas quedan marcadas, sin perder filas.
m <- funciones$marcarVueltasNoRepresentativas(v, funciones$intervalosVacios(), FALSE)
stopifnot(nrow(m) == 6, all(grepl("control_no_verificable", m$motivos_exclusion)))
# La vuelta siguiente de OTRA sesion no convierte la vuelta en entrada a boxes.
v2 <- v[1:2, ]; v2$session_key[2] <- 2L; v2$lap_number[2] <- 2L; v2$is_pit_out_lap[2] <- 1L
v2$lap_number[1] <- 1L; v2$is_pit_out_lap[1] <- 0L
stopifnot(!funciones$marcarVueltasNoRepresentativas(v2, funciones$intervalosVacios(), TRUE)$entrada_boxes_inferida[1])
stopifnot(nrow(funciones$marcarVueltasNoRepresentativas(v[FALSE, ], iv, TRUE)) == 0)

# --- Residuos atipicos (tiempos SINTETICOS, no son de F1) ---
# Stint con pendiente exacta de 0,1 s por vuelta y ruido pequeno alternado.
crearStint <- function(sesion, stint, nVueltas, desplazamiento = 0) {
  edad <- 0:(nVueltas - 1)
  data.frame(session_key = sesion, driver_number = 1L, stint_number = stint,
             edad_inicio = edad, candidata_ajuste = TRUE,
             lap_duration = 90 + 0.1 * edad + rep(c(-0.05, 0.05), length.out = nVueltas) +
               desplazamiento)
}
base <- rbind(crearStint(1L, 1L, 10), crearStint(1L, 2L, 10, 1))
base$lap_duration[5] <- base$lap_duration[5] + 5      # vuelta lenta
base$lap_duration[15] <- base$lap_duration[15] - 3    # vuelta rapida
res <- funciones$marcarResiduosAtipicos(base)
stopifnot(res$residuo_lento[5], res$residuo_rapido[15],
          sum(res$residuo_lento | res$residuo_rapido) == 2,
          all(res$origen_escala == "practica"),
          sum(res$apta_ajuste) == 18)
# Stint corto: menos de 4 candidatas, sin residuo.
corto <- rbind(base, crearStint(1L, 3L, 3))
res <- funciones$marcarResiduosAtipicos(corto)
stopifnot(all(res$estado_residuo[21:23] == "stint_corto"), all(is.na(res$residuo[21:23])),
          !any(res$apta_ajuste[21:23]))
# Practica con pocos residuos: usa la escala de las tres practicas.
pocos <- rbind(base, crearStint(2L, 1L, 6))
res <- funciones$marcarResiduosAtipicos(pocos)
stopifnot(all(res$origen_escala[res$session_key == 2] == "tres_practicas"))
# Si tampoco alcanzan las tres practicas, no se evalua ni se marca.
res <- funciones$marcarResiduosAtipicos(crearStint(1L, 1L, 6))
stopifnot(all(res$estado_residuo == "no_evaluada_pocos_residuos"),
          !any(res$residuo_lento | res$residuo_rapido), all(res$apta_ajuste))
# MAD = 0 (recta perfecta): no se evalua automaticamente.
perfecta <- rbind(crearStint(1L, 1L, 10), crearStint(1L, 2L, 10))
perfecta$lap_duration <- 90 + 0.1 * perfecta$edad_inicio
res <- funciones$marcarResiduosAtipicos(perfecta)
stopifnot(all(res$estado_residuo == "no_evaluada_pocos_residuos"))
# Las no candidatas no se ajustan; una edad desconocida impide ajustar su stint.
nc <- base; nc$candidata_ajuste[1:10] <- FALSE; nc$edad_inicio[11] <- NA
res <- funciones$marcarResiduosAtipicos(nc)
stopifnot(all(res$estado_residuo[1:10] == "no_candidata"),
          all(res$estado_residuo[11:20] == "edad_desconocida"))
stopifnot(nrow(funciones$marcarResiduosAtipicos(base[FALSE, ])) == 0)

# --- Ajuste robusto de Theil-Sen (datos SINTETICOS) ---
a <- funciones$ajustarTheilSen(0:9, 90 + 0.1 * (0:9))
stopifnot(abs(a$pendiente - 0.1) < 1e-9, abs(a$base - 90) < 1e-9)
# Una vuelta 12 s mas lenta al final no arrastra la recta, a diferencia de lm().
tiempo <- 90 + 0.1 * (0:9); tiempo[9] <- tiempo[9] + 12
a <- funciones$ajustarTheilSen(0:9, tiempo)
stopifnot(abs(a$pendiente - 0.1) < 1e-9,
          abs(coef(lm(tiempo ~ I(0:9)))[2] - 0.1) > 0.3)
# Un stint de 5 vueltas que alterna empuje y enfriamiento: tras marcar los
# atipicos le quedan menos de 4 aptas y se excluye completo.
mezclado <- crearStint(1L, 3L, 5)
mezclado$lap_duration[c(2, 4)] <- mezclado$lap_duration[c(2, 4)] + 40
res <- funciones$marcarResiduosAtipicos(rbind(base, mezclado))
filasMezcla <- 21:25
stopifnot(all(res$estado_residuo[filasMezcla] == "stint_insuficiente"),
          !any(res$apta_ajuste[filasMezcla]),
          res$residuo_lento[22], res$residuo_lento[24],
          sum(res$apta_ajuste[1:20]) == 18)

# --- Correccion de combustible (datos SINTETICOS) ---
# Stint de 4 vueltas (9 a 12) con 2 kg/vuelta, 0,03 s/kg y 1 vuelta de margen:
# cargas al empezar cada vuelta 10, 8, 6 y 4 kg.
comb <- list(sensibilidad = 0.03, consumo = 2, factorQualy = 1.25, maxVueltasQualy = 3,
             margenVueltas = 1)
cc <- data.frame(lap_number = 9:12, lap_start_stint = 9L, lap_end_stint = 12L,
                 tyre_age_at_start = 3L, compound = "MEDIUM",
                 lap_duration = c(100, 98, 98, 98))
cc$edad_inicio <- cc$tyre_age_at_start + (cc$lap_number - cc$lap_start_stint)
cc <- funciones$corregirCombustible(cc, comb)
stopifnot(identical(cc$vueltas_desde_salida, 0:3),
          isTRUE(all.equal(cc$carga_estimada, c(10, 8, 6, 4))),
          isTRUE(all.equal(cc$tiempo_corregido, c(99.7, 97.76, 97.82, 97.88))),
          !any(cc$simulacro_qualy),
          identical(cc$lap_duration, c(100, 98, 98, 98)))  # no altera el original
# Stint corto de SOFT: simulacro de clasificacion, consume 1,25 veces mas.
q <- data.frame(lap_number = 1:3, lap_start_stint = 1L, lap_end_stint = 3L,
                compound = "SOFT", lap_duration = 90)
q <- funciones$corregirCombustible(q, comb)
stopifnot(all(q$simulacro_qualy), isTRUE(all.equal(q$carga_estimada, c(7.5, 5, 2.5) + 2)))
# Sin stint asignado no se corrige.
sinStint <- funciones$corregirCombustible(data.frame(lap_number = 5L,
  lap_start_stint = NA_integer_, lap_end_stint = NA_integer_, compound = NA_character_,
  lap_duration = 90), comb)
stopifnot(is.na(sinStint$tiempo_corregido))
# Con sensibilidad 0 la correccion no cambia nada; un valor invalido se rechaza.
comb0 <- comb; comb0$sensibilidad <- 0
stopifnot(identical(funciones$corregirCombustible(cc, comb0)$tiempo_corregido, cc$lap_duration))
combMal <- comb; combMal$consumo <- -1
stopifnot(inherits(try(funciones$corregirCombustible(cc, combMal), silent = TRUE), "try-error"),
          inherits(try(funciones$corregirCombustible(cc, list(sensibilidad = 0.03)),
                       silent = TRUE), "try-error"))
# Dentro de un stint la correccion suma sensibilidad x consumo a la pendiente.
recta <- data.frame(lap_number = 1:10, lap_start_stint = 1L, lap_end_stint = 10L,
                    compound = "MEDIUM", lap_duration = 90 - 0.02 * (0:9))
recta$edad_inicio <- 0:9
r0 <- funciones$ajustarTheilSen(recta$edad_inicio, recta$lap_duration)
r1 <- funciones$ajustarTheilSen(recta$edad_inicio,
                                funciones$corregirCombustible(recta, comb)$tiempo_corregido)
stopifnot(abs((r1$pendiente - r0$pendiente) - 0.06) < 1e-9)

# --- Vuelta lenta (datos SINTETICOS) ---
# Tanda que alterna empuje y enfriamiento: las de enfriamiento superan el 110 %.
vl <- data.frame(session_key = 1L, driver_number = 1L, lap_number = 1:6,
                 date_start = format(inicio + 120 * (0:5), "%Y-%m-%dT%H:%M:%OS+00:00"),
                 lap_duration = c(110, 90, 140, 91, 135, 92),
                 is_pit_out_lap = c(1L, 0L, 0L, 0L, 0L, 0L))
vl <- funciones$anotarVueltasPiloto(vl, s, inicio, inicio + 3600)
vl <- funciones$marcarVueltasNoRepresentativas(vl, funciones$intervalosVacios(), TRUE)
stopifnot(identical(vl$vuelta_lenta, c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)),
          !vl$vuelta_lenta[1])  # la salida de boxes no se compara ni es referencia
vl2 <- funciones$marcarVueltasNoRepresentativas(
  funciones$anotarVueltasPiloto(vl[, 1:6], s, inicio, inicio + 3600),
  funciones$intervalosVacios(), TRUE, umbralVueltaLenta = 2)
stopifnot(!any(vl2$vuelta_lenta))

# --- Modelo por compuesto (datos SINTETICOS) ---
# Dos stints del mismo compuesto con distinto nivel (otra carga de combustible):
# la pendiente intra-stint no se contamina con la diferencia entre stints.
stopifnot(abs(funciones$pendienteIntraStint(c(0:4, 0:4), c(90 + 0.1 * 0:4, 95 + 0.1 * 0:4),
                                            rep(1:2, each = 5)) - 0.1) < 1e-9,
          is.na(funciones$pendienteIntraStint(c(1, 2), c(90, 91), c(1, 2))))
crearAptas <- function(dorsal, compuesto, nVueltas, base, pendiente, stint = 1L, sesion = 1L) {
  data.frame(session_key = sesion, driver_number = dorsal, stint_number = stint,
             compound = compuesto, edad_inicio = 0:(nVueltas - 1),
             tiempo_corregido = base + pendiente * (0:(nVueltas - 1)), apta_ajuste = TRUE)
}
# Campo de 4 pilotos (1 a 4): SOFT degrada 0,2 %/vuelta y MEDIUM 0,1 %/vuelta en todos.
# El HARD lo usan solo los pilotos 2, 3 y 4, siempre 1 % mas lento que su MEDIUM.
campo <- do.call(rbind, lapply(1:4, function(d) {
  b <- 90 + d
  rbind(crearAptas(d, "SOFT", 10, b * 0.99, b * 0.99 * 0.002, stint = 1L),
        crearAptas(d, "MEDIUM", 10, b, b * 0.001, stint = 2L),
        if (d > 1) crearAptas(d, "HARD", 4, b * 1.01, b * 1.01 * 0.0005, stint = 3L))
}))
aptasCampo <- funciones$aptasCompuestos(campo)
deg <- funciones$degradacionCampo(aptasCampo, 8, 5, 3)
stopifnot(isTRUE(all.equal(deg$degradacion_pct[1:2], c(0.2, 0.1))),
          identical(deg$pilotos, c(4L, 4L, 0L)), is.na(deg$degradacion_pct[3]))
bases <- funciones$basesCampo(aptasCampo, deg, 3)
# La base quita la degradacion iterando dos veces: coincide salvo por diferencias minimas.
# El HARD no tiene degradacion del campo: no hay bases de HARD.
stopifnot(!any(bases$compound == "HARD"),
          isTRUE(all.equal(bases$base[bases$driver_number == 1 & bases$compound == "MEDIUM"], 91, tolerance = 1e-5)))
# Sin degradacion de HARD en el campo se usa la del MEDIUM; sin base propia, igual al medio.
mod <- funciones$modeloPiloto(1L, bases, deg)
stopifnot(all(mod$suficiente),
          identical(mod$origen_base, c("propia", "propia", "igual_medio")),
          identical(mod$origen_pendiente, c("campo", "campo", "campo_medio")),
          isTRUE(all.equal(mod$base, c(91 * 0.99, 91, 91), tolerance = 1e-5)),
          isTRUE(all.equal(mod$pendiente, mod$base * c(0.002, 0.001, 0.001))))
# Con 3 pilotos con HARD y MEDIUM (8 vueltas de HARD) la base sale del cociente pareado.
campo8 <- rbind(campo[campo$compound != "HARD", ], do.call(rbind, lapply(2:4, function(d)
  crearAptas(d, "HARD", 8, (90 + d) * 1.01, (90 + d) * 1.01 * 0.0005, stint = 3L))))
a8 <- funciones$aptasCompuestos(campo8)
deg8 <- funciones$degradacionCampo(a8, 8, 5, 3)
mod8 <- funciones$modeloPiloto(1L, funciones$basesCampo(a8, deg8, 3), deg8)
stopifnot(mod8$origen_base[3] == "campo_pareado", mod8$pilotos_base[3] == 3,
          isTRUE(all.equal(mod8$base[3], 91 * 1.01, tolerance = 1e-5)),
          isTRUE(all.equal(mod8$degradacion_pct[3], 0.05)))
# Con un solo piloto con HARD: se usa el companero si lo es; si no, igual al medio.
b1 <- funciones$basesCampo(a8[!(a8$compound == "HARD" & a8$driver_number != 2), ], deg8, 3)
stopifnot(funciones$modeloPiloto(1L, b1, deg8, companeros = 2L)$origen_base[3] == "companero",
          funciones$modeloPiloto(1L, b1, deg8)$origen_base[3] == "igual_medio")
# Sin base propia de MEDIUM no hay referencia: el HARD queda insuficiente.
sinMedio <- bases[!(bases$driver_number == 1 & bases$compound == "MEDIUM"), ]
stopifnot(!funciones$modeloPiloto(1L, sinMedio, deg)$suficiente[3])
# Pendiente negativa: se usa la minima.
degNeg <- deg; degNeg$degradacion_pct[2] <- -0.05
stopifnot(funciones$modeloPiloto(1L, bases, degNeg)$pendiente_usada[2] == 0)

# Modelo calibrado: SOFT y HARD salen del MEDIUM propio; la degradacion se escala.
cal <- list(factorDegradacion = c(SOFT = 0.5, MEDIUM = 0.5, HARD = 0.5),
            difBase = c(SOFT = -1, HARD = 0.5), relacionDegradacion = c(SOFT = 2, HARD = 0.5))
mc <- funciones$modeloPilotoCalibrado(1L, bases, deg, cal)
bM <- bases$base[bases$driver_number == 1 & bases$compound == "MEDIUM"]
stopifnot(all(mc$suficiente),
          identical(mc$origen_base, c("medio_calibrado", "propia", "medio_calibrado")),
          identical(mc$origen_pendiente, c("campo_calibrado", "campo_calibrado", "medio_calibrado")),
          isTRUE(all.equal(mc$base, bM * c(0.99, 1, 1.005))),
          isTRUE(all.equal(mc$degradacion_pct, c(0.1, 0.05, 0.025))))  # HARD: 0,1 x 0,5 x 0,5
stopifnot(!any(funciones$modeloPilotoCalibrado(1L, sinMedio, deg, cal)$suficiente))
# Correccion de nivel: desplaza las tres bases; la de practica queda como estaba.
calN <- cal; calN$nivelBase <- -1
mn <- funciones$modeloPilotoCalibrado(1L, bases, deg, calN)
stopifnot(isTRUE(all.equal(mn$base, mc$base * 0.99)),
          isTRUE(all.equal(mn$base_practica, mc$base_practica)),
          isTRUE(all.equal(mn$degradacion_pct, mc$degradacion_pct)))

# --- Estrategias (modelo SINTETICO, no son datos de F1) ---
stopifnot(funciones$tiempoStint(90, 0.1, 0, 3) == 90 + 90.1 + 90.2,
          funciones$tiempoStint(90, 0.1, 2, 2) == 90.2 + 90.3)
# Caida desde la edad 1 con 0,5 s/vuelta^2: edades 0, 1, 2, 3 suman 0 + 0 + 0,5 + 2.
stopifnot(isTRUE(all.equal(funciones$tiempoStint(90, 0, 0, 4, 1, 0.5), 360 + 2.5)),
          isTRUE(all.equal(funciones$tiempoStint(90, 0, 0, c(2, 4), 1, 0.5), c(180, 362.5))))
mCaida <- funciones$agregarCaida(data.frame(compound = c("SOFT", "MEDIUM", "HARD"),
  degradacion_pct = c(0.1, 0.05, 0)), list(forma = "desgaste",
  umbral = c(SOFT = 2, MEDIUM = 2, HARD = 2), coef = c(SOFT = 100, MEDIUM = 100, HARD = 100)))
stopifnot(isTRUE(all.equal(mCaida$caida_edad, c(20, 40, Inf))),
          isTRUE(all.equal(mCaida$caida_coef, c(1, 0.25, 0))),
          all(funciones$agregarCaida(mCaida, NULL)$caida_coef == 0))
modelo <- data.frame(compound = c("SOFT", "MEDIUM", "HARD"), suficiente = TRUE,
                     base = c(90, 90.5, 91), pendiente = c(0.3, 0.12, 0.05),
                     pendiente_usada = c(0.3, 0.12, 0.05), edad_max_observada = 15)
est <- funciones$generarEstrategias(40, modelo, 20, paradas = 1:2, minVueltasStint = 5)
stints1 <- est[est$paradas == 1, ]
stopifnot(all(sapply(strsplit(est$secuencia, "-"), function(x) length(unique(x)) >= 2)),
          all(sapply(strsplit(est$stints, "\\+"), function(x) all(as.numeric(x) >= 5))),
          all(sapply(strsplit(est$stints, "\\+"), function(x) sum(as.numeric(x)) == 40)),
          nrow(stints1) == 6 * 31,                      # 6 secuencias x paradas 5..35
          !is.unsorted(est$tiempo_modelo))
# La mejor coincide con una busqueda directa independiente.
mejorDirecta <- Inf
for (a in c("SOFT", "MEDIUM", "HARD")) for (b in c("SOFT", "MEDIUM", "HARD")) {
  if (a == b) next
  for (L in 5:35) {
    t <- 20 + funciones$tiempoEstrategia(c(a, b), c(L, 40 - L), modelo, 0)
    mejorDirecta <- min(mejorDirecta, t)
  }
}
stopifnot(abs(min(stints1$tiempo_modelo) - mejorDirecta) < 1e-9)
# Limite de extrapolacion: con edad maxima 5 y factor 3, ningun stint pasa de 15 vueltas.
corto <- modelo; corto$edad_max_observada <- 5
estCorto <- funciones$generarEstrategias(40, corto, 20, 1:2, 5, maxFactorEdad = 3)
stopifnot(all(sapply(strsplit(estCorto$stints, "\\+"), function(x) all(as.numeric(x) - 1 <= 15))),
          all(estCorto$paradas == 2), all(estCorto$extrapola))
# Limite de vida: con 1,5 vueltas de edad inicial y vida 20, ningun stint de SOFT pasa de 18.
estVida <- funciones$generarEstrategias(40, modelo, 20, 1:2, 5, c(SOFT = 1.5, MEDIUM = 0, HARD = 0),
                                        vidaMaxima = c(SOFT = 20, MEDIUM = 40, HARD = 40))
largoSoft <- unlist(lapply(seq_len(nrow(estVida)), function(i) {
  l <- as.numeric(strsplit(estVida$stints[i], "\\+")[[1]]); l[strsplit(estVida$secuencia[i], "-")[[1]] == "SOFT"] }))
stopifnot(length(largoSoft) > 0, max(largoSoft) == 18)
# Las mejores distintas no repiten la misma combinacion de compuestos en otro orden.
mej <- funciones$mejoresEstrategias(est, 3)
comb <- sapply(strsplit(mej$secuencia, "-"), function(x) paste(sort(x), collapse = "-"))
stopifnot(nrow(mej) == 3, !anyDuplicated(paste(mej$paradas, comb)), mej$diferencia[1] == 0)
stopifnot(funciones$puestoEstrategia(min(est$tiempo_modelo), est) == 1,
          funciones$puestoEstrategia(max(est$tiempo_modelo) + 1, est) == nrow(est) + 1)
# Un compuesto insuficiente no aparece en ninguna estrategia.
sinSoft <- modelo; sinSoft$suficiente[1] <- FALSE
stopifnot(!any(grepl("SOFT", funciones$generarEstrategias(40, sinSoft, 20)$secuencia)))

# Margen de empate: sin residuos (todos 0) el modelo no cambia y el margen es 0;
# con residuos, el margen es positivo y reproducible con la misma semilla.
modeloM <- cbind(modelo, degradacion_pct = 100 * modelo$pendiente / modelo$base)
rec <- list(compuestos = strsplit(est$secuencia[1], "-")[[1]],
            largos = as.numeric(strsplit(est$stints[1], "\\+")[[1]]))
ceros <- list(degradacion = list(SOFT = 0, MEDIUM = 0, HARD = 0), difBase = list(SOFT = 0, HARD = 0))
m0 <- funciones$margenEmpate(modeloM, 40, 20, ceros, rec, simulaciones = 5)
stopifnot(abs(m0$margen) < 1e-9)
ruido <- list(degradacion = list(SOFT = c(-0.3, 0.3), MEDIUM = c(-0.3, 0.3), HARD = c(-0.3, 0.3)),
              difBase = list(SOFT = c(-1, 1), HARD = c(-1, 1)))
m1 <- funciones$margenEmpate(modeloM, 40, 20, ruido, rec, real = rec, simulaciones = 30)
m2 <- funciones$margenEmpate(modeloM, 40, 20, ruido, rec, real = rec, simulaciones = 30)
stopifnot(m1$margen > 0, identical(m1$perdidas, m2$perdidas), m1$prob_real_mejor == 0)
emp <- funciones$combinacionesEmpatadas(est, 0)
stopifnot(length(emp) == 1, funciones$combinacionesEmpatadas(est, 1e6) |> length() > 1)

# --- Neutralizaciones (datos SINTETICOS) ---
ctrl <- data.frame(
  date = format(inicio + c(0, 100, 200, 300, 400, 900, 1000, 1500), "%Y-%m-%dT%H:%M:%OS+00:00"),
  lap_number = c(1, 5, 6, 10, 11, 20, 21, 40),
  flag = c("GREEN", NA, NA, NA, NA, NA, NA, "CHEQUERED"),
  message = c("GREEN LIGHT", "VIRTUAL SAFETY CAR DEPLOYED", "VIRTUAL SAFETY CAR ENDING",
              "VSC DEPLOYED", "SAFETY CAR DEPLOYED", "SAFETY CAR IN THIS LAP",
              "SAFETY CAR DEPLOYED", "CHEQUERED FLAG"))
ep <- funciones$episodiosNeutralizacion(ctrl)
stopifnot(nrow(ep) == 4, identical(ep$tipo, c("VSC", "VSC", "SC", "SC")),
          identical(ep$cierre, c("mensaje_de_fin", "conversion_a_SC", "mensaje_de_fin", "fin_de_carrera")),
          ep$vueltas[3] == 20 - 11 + 1, ep$convertido[2], nrow(funciones$episodiosNeutralizacion(ctrl[1, ])) == 0)
prev <- data.frame(circuito_key = c(1, 1, 2, 2, 2, 2), sc = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
                   vsc = c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE))
pn <- funciones$probabilidadNeutralizacion(prev, 1, k = c(SC = 2, VSC = Inf))
stopifnot(isTRUE(all.equal(pn$p[["SC"]], (2 + 2 * 2/6) / 4)), isTRUE(all.equal(pn$p[["VSC"]], 2/6)),
          isTRUE(all.equal(sum(pn$escenarios), 1)), pn$ediciones_circuito == 2)
mom <- funciones$momentoNeutralizacion(data.frame(tipo = c("SC", "SC", "VSC"), vuelta_inicio = c(1, 50, 30),
                                                  vueltas_carrera = 50))
stopifnot(isTRUE(all.equal(mom[, "SC"], c(0.5, 0, 0, 0, 0.5))), isTRUE(all.equal(colSums(mom), c(SC = 1, VSC = 1))))
# Seguir desde la largada con MEDIUM coincide con la mejor estrategia que empieza con MEDIUM.
cont <- funciones$mejorContinuacion(40, "MEDIUM", 0, 0, "MEDIUM", FALSE, modelo, 20, minVueltasStint = 5)
directa <- min(est$tiempo_modelo[startsWith(est$secuencia, "MEDIUM-")])
stopifnot(abs(cont$tiempo - directa) < 1e-9)
# Con una parada gratis justo cuando el plan necesita parar, conviene parar.
plan <- list(compuestos = c("MEDIUM", "HARD"), vueltasParada = 20)
d0 <- funciones$decisionNeutralizacion(plan, 19, "SC", 40, modelo, 20, c(SC = 0, VSC = 0.5))
stopifnot(d0$accion == "parar", d0$ahorro > 0)
# Sin descuento (perdida igual a la verde) parar en la vuelta 2 no conviene.
d1 <- funciones$decisionNeutralizacion(plan, 2, "SC", 40, modelo, 20, c(SC = 1, VSC = 1))
stopifnot(d1$accion == "seguir")
dec <- do.call(rbind, lapply(1:35, function(L) funciones$decisionNeutralizacion(plan, L, "SC", 40, modelo, 20, c(SC = 0.5, VSC = 0.6))))
ven <- funciones$ventanasNeutralizacion(dec, 40)
stopifnot(sum(ven$consecutivas$hasta - ven$consecutivas$desde + 1) == 35, nrow(ven$secciones) == 9)
dd <- funciones$medidaD(plan, dec, 40, 20, c(SC = 0.5, VSC = 0.6), c(SC = 0.5), cbind(SC = rep(0.2, 5)))
stopifnot(dd$esperado >= 0, all(dd$porVuelta$D >= 0), isTRUE(all.equal(sum(dd$porVuelta$peso), 0.5 * 35 / 40 * 1, tolerance = 0.2)))

# --- Perdida por parada (datos SINTETICOS) ---
# Vueltas de 90 s; entrada 95 s y salida 110 s en la vuelta 10: perdida = 95 + 110 - 180 = 25.
pp <- data.frame(driver_number = 1L, lap_number = 1:20, lap_duration = 90,
                 is_pit_out_lap = 0L, cambio_neumatico = FALSE, neutralizada = FALSE)
pp$lap_duration[9:10] <- c(95, 110)
pp$is_pit_out_lap[10] <- 1L
pp$cambio_neumatico[10] <- TRUE
stopifnot(funciones$calcularPerdidasParadas(pp) == 25)
pp$neutralizada[10] <- TRUE   # parada bajo neutralizacion: no se usa
stopifnot(length(funciones$calcularPerdidasParadas(pp)) == 0)
pp$neutralizada[10] <- FALSE; pp$cambio_neumatico[10] <- FALSE   # sin cambio de neumatico
stopifnot(length(funciones$calcularPerdidasParadas(pp)) == 0)

# --- Prediccion y comparacion con la carrera (datos SINTETICOS) ---
real <- data.frame(compound = c("MEDIUM", "HARD"), lap_start = c(1, 21), lap_end = c(20, 40),
                   tyre_age_at_start = c(0, 2))
# 2 kg/vuelta, 0,03 s/kg y 1 vuelta de margen: en la vuelta 21 de 40 quedan 20 + 1 vueltas.
pred <- funciones$predecirVueltas(real, modelo, list(sensibilidad = 0.03, consumo = 2,
                                                     margenVueltas = 1), nVueltas = 40)
stopifnot(nrow(pred) == 40, pred$edad[21] == 2,
          isTRUE(all.equal(pred$prediccion[21], 91 + 0.05 * 2 + 0.03 * 2 * 21)))
# Observado = prediccion + 1,5 s exactos: error absoluto 1,5 y ajustado 0.
obs <- data.frame(lap_number = 2:40, lap_duration = pred$prediccion[2:40] + 1.5)
cmp <- funciones$compararVueltas(pred, obs)
stopifnot(cmp$n == 39, abs(cmp$mae_absoluto - 1.5) < 1e-9, abs(cmp$desplazamiento - 1.5) < 1e-9,
          cmp$mae_ajustado < 1e-9)

# --- Vuelta final sin duracion y bandera a cuadros ausente (datos SINTETICOS) ---
fv <- data.frame(driver_number = rep(1:2, each = 4), lap_number = rep(1:4, 2),
                 date_start = format(inicio + c(0, 90, 180, 900, 5, 95, 185, 900),
                                     "%Y-%m-%dT%H:%M:%OS+00:00"),
                 lap_duration = c(90, 90, 90, NA, 90, 90, NA, NA))
stopifnot(funciones$ultimaVueltaReal(fv) == 3)
carreraSintetica <- list(
  vueltas = fv,
  stints = data.frame(driver_number = 1:2, stint_number = 1L, compound = "MEDIUM",
                      lap_start = 1L, lap_end = 4L, tyre_age_at_start = 0L),
  control = data.frame(date = character(), flag = character()))
r1 <- funciones$estrategiaRealPiloto(carreraSintetica, 1)
stopifnot(r1$termino, r1$stints$lap_end == 3,          # la vuelta 4 fantasma no cuenta
          funciones$estrategiaRealPiloto(carreraSintetica, 2)$termino)

cat("OK: practicas, limpieza, combustible, modelo, estrategias, perdida en boxes y evaluacion con datos sinteticos.\n")
