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
  d <- data.frame(
    endpoint = rep(c("laps", "stints"), 3),
    estado = c("ok", "ok", "error", "ok", "ok", "ok"),
    filas = c(4L, 1L, 2L, 1L, 2L, 1L),
    parametros_json = rep(sprintf('{"session_key":%d}', 1:3), each = 2)
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
            r$resumen$vueltas_registradas[1] == 2)
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
cat("OK: horarios, limites, stints ambiguos, duplicados y tres practicas con SQLite sintetico.\n")
