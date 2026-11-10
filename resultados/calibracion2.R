# Practica contra carrera con degradacion de carrera ya separada del combustible.
# Uso desde el proyecto: Rscript resultados/calibracion2.R <regresion_carrera.csv> <salida.csv> <constantes.txt>
a <- commandArgs(TRUE)
invisible(capture.output(source("estimadorR.R")))
r <- read.csv(a[1])
filas <- list()
for (i in seq_len(nrow(r))) {
  gp <- gpConTramosCandidatos[gpConTramosCandidatos$meeting_key == r$meeting_key[i], , drop = FALSE]
  if (nrow(gp) != 1) next
  tramos <- revisionClima[revisionClima$meeting_key %in% gp$meeting_key & revisionClima$estado %in% c("candidato_completo", "candidato_parcial"), ]
  pil <- suppressWarnings(leerPilotosPracticas(rutaBase, tramos$session_key))
  campo <- tryCatch(campoGP(gp, pil$driver_number, tramos$session_key, revisionClima, rutaBase, parametros), error = function(e) NULL)
  if (is.null(campo)) next
  d <- campo$degradacion
  filas[[length(filas) + 1]] <- data.frame(r[i, ], prac_deg_S = d$degradacion_pct[1], prac_deg_M = d$degradacion_pct[2], prac_deg_H = d$degradacion_pct[3])
}
x <- do.call(rbind, filas); rownames(x) <- NULL
write.csv(x, a[2], row.names = FALSE)
cal <- x[!x$evaluable, ]
sink(a[3])
cat("GP de calibracion:", nrow(cal), "\n")
res <- list(); fac <- c()
for (k in c("S", "M", "H")) {
  p <- cal[[paste0("prac_deg_", k)]]; c0 <- cal[[paste0("car_deg_", k)]]
  ok <- which(is.finite(p) & is.finite(c0))
  fac[k] <- median((c0[ok] / p[ok])[p[ok] > 0.03])
  res[[k]] <- sapply(ok, function(i) { o <- setdiff(ok, i); c0[i] - median((c0[o] / p[o])[p[o] > 0.03]) * p[i] })
  cat(sprintf("deg %s: n=%d (con practica>0,03: %d) factor %.2f spearman %.2f mediana carrera %.3f sesgo residuo %+.3f MAD %.3f\n", k, length(ok), sum(p[ok] > 0.03), fac[k], cor(p[ok], c0[ok], method = "spearman"), median(c0, na.rm = TRUE), median(res[[k]]), mad(res[[k]])))
}
gap <- c(); rg <- list()
for (k in c("S", "H")) {
  c0 <- cal[[paste0("car_gap_", k)]]; ok <- which(is.finite(c0))
  gap[k] <- median(c0[ok])
  rg[[k]] <- sapply(ok, function(i) c0[i] - median(c0[setdiff(ok, i)]))
  cat(sprintf("gap %s: n=%d mediana %+.2f MAD res %.2f\n", k, length(ok), gap[k], mad(rg[[k]])))
}
m <- cal$car_deg_M
rel <- c(S = median((cal$car_deg_S / m)[m > 0.02], na.rm = TRUE), H = median((cal$car_deg_H / m)[m > 0.02], na.rm = TRUE))
cat("relacion S/M, H/M:", round(rel, 2), "\n\n")
cat("factorDegradacion = c(SOFT =", round(fac["S"], 2), ", MEDIUM =", round(fac["M"], 2), ", HARD =", round(fac["H"], 2), ")\n")
cat("difBase = c(SOFT =", round(gap["S"], 2), ", HARD =", round(gap["H"], 2), ")\n")
cat("relacionDegradacion = c(SOFT =", round(rel["S"], 2), ", HARD =", round(rel["H"], 2), ")\n")
for (k in names(res)) cat("resDeg", k, ":", paste(round(res[[k]], 3), collapse = ", "), "\n")
for (k in names(rg)) cat("resGap", k, ":", paste(round(rg[[k]], 2), collapse = ", "), "\n")
sink()
