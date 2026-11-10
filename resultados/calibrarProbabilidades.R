# Pronostico de neutralizaciones: cada carrera seca se pronostica SOLO con carreras
# anteriores (mismo circuito y todas). Se elige cuanto acercar la frecuencia del
# circuito a la tasa general (k carreras "ficticias" con la tasa general).
r <- read.csv("resultados/carreras_neutralizaciones.csv")
r <- r[r$seca, ]; r <- r[order(r$fecha), ]
r$sc <- r$n_sc > 0; r$vsc <- r$n_vsc > 0
cat("Carreras secas:", nrow(r), "| tasa SC", round(mean(r$sc), 2), "VSC", round(mean(r$vsc), 2),
    "| alguna", round(mean(r$sc | r$vsc), 2), "\n")
cat("Escenarios: ninguno", sum(!r$sc & !r$vsc), "solo VSC", sum(r$vsc & !r$sc), "solo SC", sum(r$sc & !r$vsc), "ambos", sum(r$sc & r$vsc), "\n")
pron <- function(k, tipo) {
  out <- list()
  for (i in seq_len(nrow(r))) {
    prev <- r[r$fecha < r$fecha[i], ]
    if (nrow(prev) < 10) next
    circ <- prev[prev$circuito_key == r$circuito_key[i], ]
    g <- mean(prev[[tipo]])
    p <- (sum(circ[[tipo]]) + k * g) / (nrow(circ) + k)
    out[[length(out) + 1]] <- data.frame(n_circ = nrow(circ), p = p, g = g, y = r[[tipo]][i])
  }
  do.call(rbind, out)
}
for (tipo in c("sc", "vsc")) {
  cat("\n", toupper(tipo), "\n")
  for (k in c(0, 0.5, 1, 2, 3, 5, 1e6)) {
    x <- pron(k, tipo)
    x$p <- pmin(pmax(x$p, 0.02), 0.98)   # evitar 0 y 1 absolutos
    con <- x$n_circ > 0
    cat(sprintf("k=%-6s Brier todas %.3f | con antecedentes del circuito (n=%d) %.3f | logloss %.3f\n",
      if (k > 1e5) "global" else k, mean((x$p - x$y)^2), sum(con), mean((x$p[con] - x$y[con])^2),
      -mean(x$y[con] * log(x$p[con]) + (1 - x$y[con]) * log(1 - x$p[con]))))
  }
}
# Variacion real entre circuitos: frecuencia por circuito con al menos 3 ediciones.
a <- aggregate(cbind(sc, vsc) ~ circuito, r, function(z) c(n = length(z), f = mean(z)))
a <- data.frame(circuito = a$circuito, n = a$sc[, "n"], sc = a$sc[, "f"], vsc = a$vsc[, "f"])
cat("\nCircuitos con 3+ ediciones secas:\n"); print(a[a$n >= 3, ][order(-a$sc[a$n >= 3]), ], row.names = FALSE, digits = 2)
