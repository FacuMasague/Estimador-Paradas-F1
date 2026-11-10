# Error de nivel del modelo calibrado, por piloto y GP. SOLO LECTURA.
# desplazamiento = mediana (real - prevista) en las vueltas limpias con la estrategia real.
# Uso desde el proyecto: Rscript resultados/errorNivel.R <calibracion2.csv> <salida.csv>
a <- commandArgs(TRUE)
invisible(capture.output(source("estimadorR.R")))
cal <- read.csv(a[1])
filas <- list()
for (i in seq_len(nrow(cal))) {
  gp <- gpConTramosCandidatos[gpConTramosCandidatos$meeting_key == cal$meeting_key[i], , drop = FALSE]
  if (nrow(gp) != 1) next
  carrera <- leerCarreraGP(rutaBase, gp$meeting_key)
  if (!isTRUE(carrera$verificada)) next
  tramos <- revisionClima[revisionClima$meeting_key %in% gp$meeting_key & revisionClima$estado %in% c("candidato_completo", "candidato_parcial"), ]
  pil <- suppressWarnings(leerPilotosPracticas(rutaBase, tramos$session_key))
  campo <- tryCatch(campoGP(gp, pil$driver_number, tramos$session_key, revisionClima, rutaBase, parametros), error = function(e) NULL)
  if (is.null(campo)) next
  N <- ultimaVueltaReal(carrera$vueltas)
  por <- list()
  for (d in pil$driver_number) {
    m <- modeloDePiloto(d, campo, parametros)
    if (!all(m$suficiente)) next
    real <- estrategiaRealPiloto(carrera, d)
    if (!real$compuestos_conocidos || !real$limites_consistentes) next
    v <- vueltasCarreraPiloto(carrera, d)
    v <- v[v$limpia_carrera & v$lap_number <= N, ]
    if (nrow(v) < 15) next
    v <- v[v$lap_duration <= 1.05 * median(v$lap_duration), ]
    cmp <- compararVueltas(predecirVueltas(real$stints, m, campo$combustible, N), v)
    if (cmp$n < 15) next
    por[[length(por) + 1]] <- data.frame(temporada = gp$temporada, gp = gp$nombre, evaluable = cal$evaluable[i],
      dorsal = d, t_med = median(v$lap_duration), base_M = m$base[2], desp = cmp$desplazamiento, n = cmp$n)
  }
  x <- do.call(rbind, por)
  if (is.null(x)) next
  x$pct <- 100 * x$desp / x$t_med
  x$puesto_ritmo <- rank(x$t_med)          # 1 = mas rapido en carrera
  filas[[length(filas) + 1]] <- x
  cat(gp$temporada, gp$nombre, nrow(x), "\n")
}
write.csv(do.call(rbind, filas), a[2], row.names = FALSE)
