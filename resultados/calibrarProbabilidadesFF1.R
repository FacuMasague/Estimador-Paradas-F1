# Probabilidad de VSC/SC por circuito con FastF1 2019-2026 (mas ediciones por circuito).
# Cada carrera seca se pronostica SOLO con carreras anteriores. Se excluyen Estados
# Unidos, Mexico y Brasil 2026 (reservados para la evaluacion externa).
v <- read.csv("resultados/ff1/estado_vueltas.csv")
v$lugar <- trimws(v$lugar)
for (c in c("sc", "vsc", "roja", "mojado", "lluvia")) v[[c]] <- v[[c]] %in% c(TRUE, "True", "TRUE")
v$lugar[v$lugar == "Miami Gardens"] <- "Miami"; v$lugar[v$lugar == "Monte Carlo"] <- "Monaco"
v <- v[!(v$anio == 2026 & grepl("United States|Mexico|S.o Paulo", v$evento)), ]
v$clave <- paste(v$anio, v$ronda)
# Episodios: tramos de vueltas consecutivas con el mismo tipo.
ep <- do.call(rbind, lapply(split(v, v$clave), function(x) {
  x <- x[order(x$vuelta), ]; out <- list()
  for (t in c("sc", "vsc")) {
    s <- x[[t]]; ini <- which(s & !c(FALSE, head(s, -1)))
    fin <- which(s & !c(tail(s, -1), FALSE))
    if (length(ini)) out[[t]] <- data.frame(clave = x$clave[1], tipo = toupper(t), vuelta_inicio = ini, vueltas = fin - ini + 1, n = x$vueltas[1])
  }
  do.call(rbind, out)
}))
r <- aggregate(cbind(lluvia = lluvia %in% TRUE, mojado, roja) ~ clave + anio + ronda + evento + lugar + fecha + vueltas, v, any)
r$sc <- r$clave %in% ep$clave[ep$tipo == "SC"]; r$vsc <- r$clave %in% ep$clave[ep$tipo == "VSC"]
r$seca <- !r$lluvia & !r$mojado
write.csv(r, "resultados/ff1/carreras_ff1.csv", row.names = FALSE); write.csv(ep, "resultados/ff1/episodios_ff1.csv", row.names = FALSE)
s <- r[r$seca, ]; s <- s[order(s$fecha), ]
cat("Carreras:", nrow(r), " secas:", nrow(s), " circuitos:", length(unique(s$lugar)), "\n")
cat("Tasa en secas: SC", round(mean(s$sc), 2), " VSC", round(mean(s$vsc), 2), " alguna", round(mean(s$sc | s$vsc), 2), "\n")
cat("Ediciones secas por circuito:\n"); print(table(table(s$lugar)))
pron <- function(k, tipo, desde = 2021) {
  out <- list()
  for (i in which(s$anio >= desde)) {
    prev <- s[s$fecha < s$fecha[i], ]; circ <- prev[prev$lugar == s$lugar[i], ]
    g <- mean(prev[[tipo]])
    p <- if (is.infinite(k)) g else (sum(circ[[tipo]]) + k * g) / (nrow(circ) + k)
    out[[length(out) + 1]] <- data.frame(n = nrow(circ), p = min(max(p, .02), .98), y = s[[tipo]][i])
  }
  do.call(rbind, out)
}
for (tipo in c("sc", "vsc")) {
  cat("\n", toupper(tipo), "(pronostico de carreras secas 2021-2026)\n")
  for (k in c(1, 2, 3, 5, 8, 12, Inf)) {
    x <- pron(k, tipo); c3 <- x$n >= 3
    cat(sprintf("k=%-4s Brier %.3f (n=%d) | con 3+ ediciones previas %.3f (n=%d)\n", k, mean((x$p - x$y)^2), nrow(x), mean((x$p[c3] - x$y[c3])^2), sum(c3)))
  }
}
# Hay diferencias reales entre circuitos? Prueba de independencia (simulada).
for (tipo in c("sc", "vsc")) {
  tab <- table(s$lugar, s[[tipo]]); tab <- tab[rowSums(tab) >= 4, ]
  cat(sprintf("\n%s: circuitos con 4+ ediciones %d | chi2 simulado p = %.3f\n", toupper(tipo), nrow(tab), suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 20000)$p.value)))
}
a <- aggregate(cbind(sc, vsc) ~ lugar, s, mean); a$n <- as.numeric(table(s$lugar)[a$lugar])
cat("\nCircuitos con 5+ ediciones secas:\n"); print(a[a$n >= 5, ][order(-a$sc[a$n >= 5]), ], row.names = FALSE, digits = 2)
# Momento de inicio (5 tramos) y duracion, carreras secas.
e <- ep[ep$clave %in% s$clave, ]
e$tramo <- pmin(5, floor(5 * (e$vuelta_inicio - 1) / e$n) + 1)
cat("\nInicio por tramo de 20 % (secas):\n"); print(table(e$tipo, e$tramo))
cat("Duracion en vueltas (secas):\n"); print(t(sapply(split(e$vueltas, e$tipo), quantile, c(.25, .5, .75))))
