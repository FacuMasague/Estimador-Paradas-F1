# Error de nivel ampliado: todas las carreras secas, incluidos los fines de semana Sprint.
# Solo para medir; no cambia el programa. Respecto de errorNivel.R:
# - Se aceptan fines de semana con una sola practica (Sprint).
# - Si el campo no alcanza para una degradacion, se usa el valor tipico de carrera.
# - Sin MEDIUM propio, la referencia sale de otro compuesto propio y difBase.
# Uso desde el proyecto: Rscript resultados/errorNivel2.R <salida.csv>
a <- commandArgs(TRUE)
invisible(capture.output(source("estimadorR.R")))
cal <- parametros$calibracion
cal$nivelBase <- 0   # se mide el error SIN la correccion de nivel
# El flujo principal filtra el clima a los GP candidatos: aca se lee completo.
con <- dbConnect(SQLite(), rutaBase, flags = SQLITE_RO)
climaTodo <- dbGetQuery(con, "SELECT session_key, registro, date, rainfall, track_temperature FROM clima ORDER BY session_key, date, registro")
descargasTodo <- dbGetQuery(con, "SELECT json_extract(parametros_json, '$.session_key') AS session_key, estado, filas, exito_en, error FROM descargas WHERE endpoint = 'weather'")
dbDisconnect(con)
tipica <- c(SOFT = 0.053 * cal$relacionDegradacion[["SOFT"]], MEDIUM = 0.053, HARD = 0.053 * cal$relacionDegradacion[["HARD"]])   # deg de carrera, %
carreras <- sesiones[sesiones$nombre == "Race" & sesiones$presente_en_fuente %in% 1, ]
filas <- list()
for (i in seq_len(nrow(carreras))) {
  m <- carreras$meeting_key[i]
  carrera <- leerCarreraGP(rutaBase, m)
  if (!isTRUE(carrera$verificada) || nrow(carrera$vueltas) == 0) next
  f <- aFechaUTC(carrera$clima$date)
  ll <- carrera$clima$rainfall[f >= aFechaUTC(carrera$sesion$fecha_inicio) & f <= aFechaUTC(carrera$sesion$fecha_fin)]
  if (length(ll) == 0 || !all(ll %in% 0) || any(carrera$stints$compound %in% c("INTERMEDIATE", "WET"))) next
  info <- grandesPremios[grandesPremios$meeting_key == m, ]
  gp <- data.frame(meeting_key = m, temporada = info$temporada, nombre = info$nombre)
  sprint <- !m %in% gpConTramosCandidatos$meeting_key
  rc <- revisionClima
  if (sprint) {
    pr <- sesiones[sesiones$meeting_key == m & sesiones$nombre %in% nombresPracticas & sesiones$presente_en_fuente %in% 1, ]
    if (nrow(pr) == 0) next
    filasC <- lapply(seq_len(nrow(pr)), function(k) cbind(pr[k, c("meeting_key", "session_key", "nombre")],
      revisarClimaPractica(pr[k, ], climaTodo, descargasTodo, maxHuecoSegundos = 120)))
    ex <- do.call(rbind, filasC)
    faltan <- setdiff(nombresPracticas, ex$nombre)
    if (length(faltan)) {
      r0 <- ex[rep(1, length(faltan)), ]
      r0$nombre <- faltan; r0$session_key <- -seq_along(faltan); r0$estado <- "sin_sesion"
      ex <- rbind(ex, r0)
    }
    rc <- ex
  }
  tramos <- rc[rc$meeting_key %in% m & rc$estado %in% c("candidato_completo", "candidato_parcial"), ]
  if (nrow(tramos) == 0) next
  pil <- suppressWarnings(leerPilotosPracticas(rutaBase, tramos$session_key))
  if (nrow(pil) == 0) next
  campo <- tryCatch(campoGP(gp, pil$driver_number, tramos$session_key, rc, rutaBase, parametros), error = function(e) NULL)
  if (is.null(campo)) next
  deg <- campo$degradacion
  usoTipica <- !is.finite(deg$degradacion_pct)
  deg$degradacion_pct[usoTipica] <- (tipica / cal$factorDegradacion)[deg$compound[usoTipica]]
  deg$edad_max_observada[usoTipica] <- 30
  bases <- basesCampo(campo$aptas, deg, parametros$minAptasBase)
  N <- ultimaVueltaReal(carrera$vueltas)
  evaluable <- estadoCarreraEvaluable(carrera) == "ok"
  por <- list()
  for (d in pil$driver_number) {
    b <- bases
    if (!any(b$driver_number == d & b$compound == "MEDIUM")) {
      otra <- b[b$driver_number == d, ]
      if (nrow(otra) == 0) next
      otra <- otra[which.max(otra$vueltas), ]
      b <- rbind(b, data.frame(driver_number = d, compound = "MEDIUM",
                               base = otra$base / (1 + cal$difBase[[otra$compound]] / 100), vueltas = 0L))
    }
    mo <- modeloPilotoCalibrado(d, b, deg, cal, parametros$pendienteMinima)
    real <- estrategiaRealPiloto(carrera, d)
    if (!real$compuestos_conocidos || !real$limites_consistentes) next
    v <- vueltasCarreraPiloto(carrera, d)
    v <- v[v$limpia_carrera & v$lap_number <= N, ]
    if (nrow(v) < 15) next
    v <- v[v$lap_duration <= 1.05 * median(v$lap_duration), ]
    cmp <- compararVueltas(predecirVueltas(real$stints, mo, campo$combustible, N), v)
    if (cmp$n < 15) next
    por[[length(por) + 1]] <- data.frame(temporada = gp$temporada, gp = gp$nombre, evaluable = evaluable,
      sprint = sprint, deg_tipica = sum(usoTipica), dorsal = d, t_med = median(v$lap_duration),
      referencia_propia = any(bases$driver_number == d & bases$compound == "MEDIUM"),
      desp = cmp$desplazamiento, n = cmp$n)
  }
  x <- do.call(rbind, por)
  if (is.null(x)) next
  x$pct <- 100 * x$desp / x$t_med
  filas[[length(filas) + 1]] <- x
  cat(gp$temporada, gp$nombre, if (sprint) "(Sprint)", nrow(x), "\n")
}
write.csv(do.call(rbind, filas), a[1], row.names = FALSE)
