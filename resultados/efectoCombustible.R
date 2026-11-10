# Separa combustible y degradacion en carrera. SOLO LECTURA.
# t = piloto + compuesto + beta_c * edad + gamma * vuelta. gamma < 0: el auto acelera por vuelta.
# Uso: Rscript efectoCombustible.R <proyecto> <calibracion.csv> <salida.csv>
a <- commandArgs(TRUE); setwd(a[1])
invisible(capture.output(source("estimadorR.R")))
cal <- read.csv(a[2])
filas <- list()
for (i in seq_len(nrow(cal))) {
  m <- cal$meeting_key[i]
  carrera <- leerCarreraGP(rutaBase, m)
  if (!isTRUE(carrera$verificada)) next
  N <- ultimaVueltaReal(carrera$vueltas)
  v <- do.call(rbind, lapply(unique(carrera$vueltas$driver_number), function(d) {
    x <- vueltasCarreraPiloto(carrera, d)
    x <- x[x$limpia_carrera & x$compound %in% compuestosSecos & is.finite(x$edad_inicio) & x$lap_number <= N,
           c("driver_number","lap_number","compound","edad_inicio","lap_duration")]
    if (nrow(x) == 0) return(NULL)
    x[x$lap_duration <= 1.05 * median(x$lap_duration), ]
  }))
  if (is.null(v) || nrow(v) < 200) next
  v$compound <- factor(v$compound)
  v$piloto <- factor(v$driver_number)
  f <- if (nlevels(v$compound) > 1) lap_duration ~ piloto + compound + compound:edad_inicio + lap_number else lap_duration ~ piloto + edad_inicio + lap_number
  ajuste <- tryCatch(lm(f, v), error = function(e) NULL)
  if (is.null(ajuste)) next
  s <- summary(ajuste)$coefficients
  if (!"lap_number" %in% rownames(s)) next
  b <- s[grep("edad_inicio", rownames(s)), , drop = FALSE]
  deg <- setNames(rep(NA_real_, 3), compuestosSecos)
  for (k in compuestosSecos) { r <- grep(k, rownames(b)); if (length(r)) deg[k] <- b[r[1], 1] }
  filas[[length(filas) + 1]] <- data.frame(temporada = cal$temporada[i], gp = cal$gp[i], evaluable = cal$evaluable[i],
    vueltas_carrera = N, n = nrow(v), gamma = s["lap_number", 1], se = s["lap_number", 2],
    deg_S = deg["SOFT"], deg_M = deg["MEDIUM"], deg_H = deg["HARD"])
}
x <- do.call(rbind, filas); rownames(x) <- NULL
write.csv(x, a[3], row.names = FALSE)
x$periodo <- ifelse(x$temporada >= 2026, "2026", "2023-25")
x$usado <- 0.030 * ifelse(x$temporada >= 2026, 70, 105) / x$vueltas_carrera
cat("Carreras:", nrow(x), "\n")
ok <- x[x$se < 0.02, ]
cat("Con estimacion precisa (se < 0,02):", nrow(ok), "\n")
for (p in unique(ok$periodo)) {
  d <- ok[ok$periodo == p, ]
  cat(sprintf("%s n=%d | gamma mediana %+.3f (cuartiles %+.3f a %+.3f) | usado en el programa -%.3f | deg S %.3f M %.3f H %.3f s/vuelta\n",
    p, nrow(d), median(d$gamma), quantile(d$gamma, .25), quantile(d$gamma, .75), median(d$usado),
    median(d$deg_S, na.rm = TRUE), median(d$deg_M, na.rm = TRUE), median(d$deg_H, na.rm = TRUE)))
}
