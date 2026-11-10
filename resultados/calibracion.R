# Practica contra carrera, por GP, para calibrar. SOLO LECTURA.
# Uso: Rscript calibracion.R <carpeta del proyecto> <salida.csv>
a <- commandArgs(TRUE); setwd(a[1])
invisible(capture.output(source("estimadorR.R")))

# Gap pareado de bases (compuesto / MEDIUM - 1, en %) por piloto; mediana del GP.
gapPareado <- function(b, cpd) {
  w <- merge(b[b$compound == cpd, c("driver_number", "base")],
             b[b$compound == "MEDIUM", c("driver_number", "base")], by = "driver_number")
  r <- 100 * (w$base.x / w$base.y - 1)
  c(gap = if (length(r) > 0) median(r) else NA_real_, n = length(r))
}

filas <- list()
for (g in seq_len(nrow(gpConTramosCandidatos))) {
  gp <- gpConTramosCandidatos[g, , drop = FALSE]
  carrera <- leerCarreraGP(rutaBase, gp$meeting_key)
  if (!isTRUE(carrera$verificada) || nrow(carrera$vueltas) == 0) next
  evaluable <- estadoCarreraEvaluable(carrera) == "ok"
  fechas <- aFechaUTC(carrera$clima$date)
  lluvia <- carrera$clima$rainfall[fechas >= aFechaUTC(carrera$sesion$fecha_inicio) &
                                     fechas <= aFechaUTC(carrera$sesion$fecha_fin)]
  if (length(lluvia) == 0 || !all(lluvia %in% 0)) next
  if (any(carrera$stints$compound %in% c("INTERMEDIATE", "WET"))) next

  tramos <- revisionClima[revisionClima$meeting_key %in% gp$meeting_key &
                            revisionClima$estado %in% c("candidato_completo", "candidato_parcial"), ]
  pil <- suppressWarnings(leerPilotosPracticas(rutaBase, tramos$session_key))
  campo <- tryCatch(campoGP(gp, pil$driver_number, tramos$session_key, revisionClima, rutaBase,
                            parametros), error = function(e) NULL)
  if (is.null(campo)) next
  comb <- campo$combustible
  N <- comb$vueltasCarrera

  # Carrera: vueltas limpias (sin vuelta 1, boxes ni neutralizacion), combustible con
  # el mismo modelo que la prediccion: carga = consumo * (N - vuelta + 1 + margen).
  st <- list()
  for (d in unique(carrera$vueltas$driver_number)) {
    v <- vueltasCarreraPiloto(carrera, d)
    v <- v[v$limpia_carrera & v$compound %in% compuestosSecos & is.finite(v$edad_inicio) &
             v$lap_number <= N, ]
    if (nrow(v) == 0) next
    v$tiempo_corregido <- v$lap_duration -
      comb$sensibilidad * comb$consumo * (N - v$lap_number + 1 + comb$margenVueltas)
    for (s in unique(v$stint_number)) {
      x <- v[v$stint_number == s, ]
      if (nrow(x) < parametros$minAptas || diff(range(x$edad_inicio)) < parametros$minRango) next
      aj <- ajustarTheilSen(x$edad_inicio, x$tiempo_corregido)
      st[[length(st) + 1]] <- data.frame(driver_number = d, compound = x$compound[1],
                                         base = aj$base, pct = 100 * aj$pendiente / aj$base)
    }
  }
  st <- do.call(rbind, st)
  if (is.null(st)) next
  porPiloto <- aggregate(cbind(base, pct) ~ driver_number + compound, st, median)

  fila <- data.frame(temporada = gp$temporada, gp = gp$nombre, meeting_key = gp$meeting_key,
                     evaluable = evaluable)
  for (cpd in compuestosSecos) {
    k <- substr(cpd, 1, 1)
    fila[[paste0("prac_deg_", k)]] <- campo$degradacion$degradacion_pct[campo$degradacion$compound == cpd]
    fila[[paste0("prac_pilotos_", k)]] <- campo$degradacion$pilotos[campo$degradacion$compound == cpd]
    r <- porPiloto$pct[porPiloto$compound == cpd]
    fila[[paste0("car_deg_", k)]] <- if (length(r) >= 3) median(r) else NA_real_
    fila[[paste0("car_pilotos_", k)]] <- length(r)
  }
  for (cpd in c("SOFT", "HARD")) {
    k <- substr(cpd, 1, 1)
    p <- gapPareado(campo$bases, cpd); r <- gapPareado(porPiloto, cpd)
    fila[[paste0("prac_gap_", k)]] <- if (p["n"] >= 2) p["gap"] else NA_real_
    fila[[paste0("prac_gap_n_", k)]] <- p["n"]
    fila[[paste0("car_gap_", k)]] <- if (r["n"] >= 2) r["gap"] else NA_real_
    fila[[paste0("car_gap_n_", k)]] <- r["n"]
  }
  filas[[length(filas) + 1]] <- fila
  cat(gp$temporada, gp$nombre, if (evaluable) "(evaluable)" else "", "\n")
}
x <- do.call(rbind, filas)
rownames(x) <- NULL
write.csv(x, a[2], row.names = FALSE)
cat("GP:", nrow(x), "\n")
