# Separa combustible y degradacion en carrera, por GP. SOLO LECTURA.
# t = piloto + compuesto + beta_c * edad + gamma * vuelta (referencia: MEDIUM).
# gamma incluye el combustible quemado y la evolucion de la pista.
# Uso desde la carpeta del proyecto: Rscript resultados/regresionCarrera.R <calibracion.csv> <salida.csv>
a <- commandArgs(TRUE)
invisible(capture.output(source("estimadorR.R")))
cal <- read.csv(a[1])
filas <- list()
for (i in seq_len(nrow(cal))) {
  carrera <- leerCarreraGP(rutaBase, cal$meeting_key[i])
  if (!isTRUE(carrera$verificada)) next
  N <- ultimaVueltaReal(carrera$vueltas)
  v <- do.call(rbind, lapply(unique(carrera$vueltas$driver_number), function(d) {
    x <- vueltasCarreraPiloto(carrera, d)
    x <- x[x$limpia_carrera & x$compound %in% compuestosSecos & is.finite(x$edad_inicio) & x$lap_number <= N,
           c("driver_number", "lap_number", "compound", "edad_inicio", "lap_duration")]
    if (nrow(x) == 0) return(NULL)
    x[x$lap_duration <= 1.05 * median(x$lap_duration), ]
  }))
  if (is.null(v) || nrow(v) < 200) next
  presentes <- intersect(c("MEDIUM", "SOFT", "HARD"), unique(v$compound))
  v$compound <- factor(v$compound, levels = presentes)
  v$piloto <- factor(v$driver_number)
  f <- if (length(presentes) > 1) lap_duration ~ piloto + compound + compound:edad_inicio + lap_number else lap_duration ~ piloto + edad_inicio + lap_number
  ajuste <- tryCatch(lm(f, v), error = function(e) NULL)
  if (is.null(ajuste)) next
  co <- summary(ajuste)$coefficients
  if (!"lap_number" %in% rownames(co)) next
  deg <- setNames(rep(NA_real_, 3), compuestosSecos); gap <- deg
  tMed <- median(v$lap_duration)
  for (k in presentes) {
    n <- if (length(presentes) > 1) paste0("compound", k, ":edad_inicio") else "edad_inicio"
    if (n %in% rownames(co)) deg[k] <- 100 * co[n, 1] / tMed
    g <- paste0("compound", k)
    if (k != "MEDIUM" && "MEDIUM" %in% presentes && g %in% rownames(co)) gap[k] <- 100 * co[g, 1] / tMed
  }
  filas[[length(filas) + 1]] <- data.frame(temporada = cal$temporada[i], gp = cal$gp[i],
    meeting_key = cal$meeting_key[i], evaluable = cal$evaluable[i], vueltas_carrera = N, t_med = tMed,
    n = nrow(v), gamma = co["lap_number", 1], se = co["lap_number", 2],
    car_deg_S = deg["SOFT"], car_deg_M = deg["MEDIUM"], car_deg_H = deg["HARD"],
    car_gap_S = gap["SOFT"], car_gap_H = gap["HARD"])
}
x <- do.call(rbind, filas); rownames(x) <- NULL
write.csv(x, a[2], row.names = FALSE)
