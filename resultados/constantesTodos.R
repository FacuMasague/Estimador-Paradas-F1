# Constantes de calibracion con TODOS los GP de tres practicas (casos incluidos).
# Usa resultados/calibracion2.csv (practica: campo del GP; carrera: regresion por GP).
x <- read.csv("resultados/calibracion2.csv")
cat("GP:", nrow(x), "(casos:", sum(x$evaluable), ")\n")
res <- list(); fac <- c()
for (k in c("S", "M", "H")) {
  p <- x[[paste0("prac_deg_", k)]]; c0 <- x[[paste0("car_deg_", k)]]
  ok <- which(is.finite(p) & is.finite(c0))
  fac[k] <- median((c0[ok] / p[ok])[p[ok] > 0.03])
  res[[k]] <- sapply(ok, function(i) { o <- setdiff(ok, i); c0[i] - median((c0[o] / p[o])[p[o] > 0.03]) * p[i] })
  cat(sprintf("deg %s: n=%d factor %.2f (MAD residuo %.3f)\n", k, sum(p[ok] > 0.03), fac[k], mad(res[[k]])))
}
gap <- c(); rg <- list()
for (k in c("S", "H")) { c0 <- x[[paste0("car_gap_", k)]]; ok <- which(is.finite(c0)); gap[k] <- median(c0[ok])
  rg[[k]] <- sapply(ok, function(i) c0[i] - median(c0[setdiff(ok, i)])); cat(sprintf("gap %s: n=%d %+.2f\n", k, length(ok), gap[k])) }
m <- x$car_deg_M
rel <- c(S = median((x$car_deg_S / m)[m > 0.02], na.rm = TRUE), H = median((x$car_deg_H / m)[m > 0.02], na.rm = TRUE))
cat("relacion:", round(rel, 2), "\n")
r <- read.csv("resultados/regresion_carrera.csv"); cat("efectoCarrera (gamma*N) todos:", round(median(-r$gamma * r$vueltas_carrera), 2), "n", nrow(r), "\n")
sink("resultados/constantes_todos.txt")
cat("factor", round(fac, 2), "\ngap", round(gap, 2), "\nrel", round(rel, 2), "\n")
for (k in names(res)) cat("resDeg", k, ":", paste(round(res[[k]], 3), collapse = ", "), "\n")
for (k in names(rg)) cat("resGap", k, ":", paste(round(rg[[k]], 2), collapse = ", "), "\n")
sink()
