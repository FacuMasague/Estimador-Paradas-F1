# Calibrar la proporcion de perdida bajo VSC/SC con las decisiones reales de los equipos.
# ahorro(f) = ahorro(f0) + (f0 - f) * perdida, porque parar paga f * perdida verde.
# Se elige f para que la regla "parar si ahorro > 0" coincida lo mas posible con lo que
# hicieron los pilotos, validando con cada GP afuera. Uso: Rscript resultados/calibrarFactores.R
invisible(capture.output(source("estimadorR.R")))
x <- read.csv("resultados/validacion_neutralizaciones.csv")
x <- x[!is.na(x$ahorro) & is.finite(x$ahorro), ]
x$clave <- paste(x$temporada, x$gp)
gps <- unique(x[, c("temporada", "gp", "clave")])
gps$perdida <- sapply(seq_len(nrow(gps)), function(i) {
  m <- gpConTramosCandidatos$meeting_key[gpConTramosCandidatos$temporada == gps$temporada[i] & gpConTramosCandidatos$nombre == gps$gp[i]]
  estimarPerdidaBoxes(rutaBase, m)$valor })
x <- merge(x, gps[, c("clave", "perdida")])
f0 <- c(SC = 0.50, VSC = 0.57)
grilla <- seq(0.2, 1.2, by = 0.01)
acuerdo <- function(d, t, f) mean((d$ahorro + (f0[[t]] - f) * d$perdida > 0) == d$paro)
res <- list()
for (t in c("SC", "VSC")) {
  d <- x[x$tipo == t, ]
  a <- sapply(grilla, function(f) acuerdo(d, t, f))
  fMejor <- grilla[which.max(a)]
  # Validacion dejando fuera cada GP.
  cv <- c(); fs <- c()
  for (g in unique(d$clave)) {
    tr <- d[d$clave != g, ]; te <- d[d$clave == g, ]
    at <- sapply(grilla, function(f) acuerdo(tr, t, f))
    ff <- median(grilla[at == max(at)]); fs <- c(fs, ff)
    cv <- c(cv, (te$ahorro + (f0[[t]] - ff) * te$perdida > 0) == te$paro)
  }
  base <- mean(!d$paro)
  cat(sprintf("%s: n=%d | f actual %.2f acuerdo %.0f%% | mejor f %.2f (rango de f con acuerdo maximo %.2f-%.2f) acuerdo %.0f%% | validado por GP %.0f%% (f entre %.2f y %.2f) | regla 'nunca parar' %.0f%%\n",
    t, nrow(d), f0[[t]], 100 * acuerdo(d, t, f0[[t]]), fMejor, min(grilla[a == max(a)]), max(grilla[a == max(a)]), 100 * max(a), 100 * mean(cv), min(fs), max(fs), 100 * base))
  res[[t]] <- data.frame(tipo = t, f = grilla, acuerdo = a)
}
write.csv(do.call(rbind, res), "resultados/calibracion_factores.csv", row.names = FALSE)
