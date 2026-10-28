# Pruebas de funcionamiento con datos SINTETICOS, no observaciones de F1.
# Ejecutar desde la carpeta del proyecto: Rscript test_practicas.R
library(DBI)
library(RSQLite)

# Cargar solamente las definiciones, sin ejecutar catalogos, menus ni consultas.
funciones <- new.env(parent = globalenv())
for (expresion in parse("estimadorR.R")) {
  if (is.call(expresion) && identical(expresion[[1]], as.name("<-")) &&
      is.call(expresion[[3]]) && identical(expresion[[3]][[1]], as.name("function"))) {
    eval(expresion, envir = funciones)
  }
}

inicio <- as.POSIXct("2023-01-01 10:00:00", tz = "UTC")
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
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta)
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
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta)
  stopifnot(r$resumen$estado_datos[3] == "no_revisado_por_clima",
            all(r$vueltas$session_key == 1))
  # Si no hay ningun tramo candidato, conservar un resumen de tres filas y tablas vacias.
  clima$estado[] <- "indeterminado"
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta)
  stopifnot(nrow(r$resumen) == 3, nrow(r$vueltas) == 0, nrow(r$stints) == 0)
  # Detectar cantidades inconsistentes sin detener las demas sesiones.
  clima$estado[] <- "candidato_completo"
  con <- dbConnect(SQLite(), ruta)
  dbExecute(con,
    "UPDATE descargas SET filas=999 WHERE endpoint='laps' AND json_extract(parametros_json, '$.session_key')=1")
  dbDisconnect(con)
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta)
  stopifnot(r$resumen$estado_datos[1] == "cantidad_de_filas_inconsistente",
            r$resumen$estado_datos[3] == "revisado")
  # El piloto puede no tener registros aunque la descarga de la sesion sea correcta.
  seleccion$piloto$driver_number <- 99L
  r <- funciones$revisarPracticasPiloto(seleccion, clima, ruta)
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

cat("OK: horarios, limites, stints ambiguos, duplicados, tres practicas, edad, marcas y residuos con datos sinteticos.\n")
