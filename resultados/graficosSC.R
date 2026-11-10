# Graficos de la validacion ante safety car: decision del programa contra lo que hizo
# cada piloto en los SC reales medibles. Lee resultados/validacion_neutralizaciones.csv.
# Uso: Source en RStudio (muestra los graficos) o Rscript resultados/graficosSC.R
# (los guarda en resultados/graficos_sc/).
x <- read.csv("resultados/validacion_neutralizaciones.csv")
x <- x[x$tipo == "SC" & !is.na(x$ahorro), ]
x$ahorro[!is.finite(x$ahorro)] <- 30
x$acuerdo <- x$recomienda_parar == x$paro
x$episodio <- sprintf("%d %s, v%d", x$temporada, sub(" Grand Prix", "", x$gp), x$vuelta)
carpeta <- "resultados/graficos_sc"
dir.create(carpeta, showWarnings = FALSE)
colAcuerdo <- c("TRUE" = "#2e7d32", "FALSE" = "#c62828")

guardar <- function(nombre, ancho, alto, dibujo) {
  png(file.path(carpeta, nombre), width = ancho, height = alto, res = 110)
  dibujo(); dev.off()
  if (interactive()) dibujo()
}

# 1. Cada SC real: un renglon por episodio y un punto por piloto.
episodios <- unique(x[order(x$temporada, x$gp, x$vuelta), "episodio"])
guardar("1_cada_sc.png", 1400, 120 + 32 * length(episodios), function() {
  op <- par(mar = c(4.5, 15, 3, 1)); on.exit(par(op))
  y <- match(x$episodio, episodios) + runif(nrow(x), -0.25, 0.25)
  plot(pmax(pmin(x$ahorro, 30), -30), y, pch = ifelse(x$paro, 19, 1), cex = 1.2,
       col = colAcuerdo[as.character(x$acuerdo)], yaxt = "n", ylab = "",
       xlab = "Ahorro estimado de parar bajo SC (s; positivo = el programa dice parar)",
       main = "Cada safety car real: decision del programa contra la del piloto", xlim = c(-30, 30))
  axis(2, at = seq_along(episodios), labels = episodios, las = 1, cex.axis = 0.8)
  abline(v = 0, lty = 2); abline(h = seq_along(episodios) + 0.5, col = "grey90")
  porEp <- tapply(x$acuerdo, x$episodio, mean)[episodios]
  text(30, seq_along(episodios), sprintf("%.0f %%", 100 * porEp), pos = 2, cex = 0.75, col = "grey30")
  legend("bottomleft", bty = "n", cex = 0.8, pch = c(19, 1, 15, 15),
         col = c("black", "black", colAcuerdo),
         legend = c("Paro", "Siguio", "Programa coincide", "Programa no coincide"))
})

# 2. Proporcion de pilotos que paro segun el ahorro estimado.
guardar("2_ahorro_vs_decision.png", 1200, 700, function() {
  bandas <- cut(pmax(pmin(x$ahorro, 30), -30), c(-31, -15, -8, -3, 0, 3, 8, 15, 31))
  p <- tapply(x$paro, bandas, mean); n <- tapply(x$paro, bandas, length)
  centro <- c(-22.5, -11.5, -5.5, -1.5, 1.5, 5.5, 11.5, 22.5)
  plot(pmax(pmin(x$ahorro, 30), -30), x$paro + runif(nrow(x), -0.03, 0.03), pch = 16, col = "#00000030",
       xlab = "Ahorro estimado de parar bajo SC (s)", ylab = "Paro (1) / siguio (0)",
       main = "Cuanto mas ahorro estima el programa, mas pilotos paran", xlim = c(-30, 30))
  lines(centro, p, type = "b", lwd = 2, pch = 19, col = "#1565c0")
  text(centro, p, sprintf("%.0f %%\n(n=%d)", 100 * p, n), pos = 3, cex = 0.7, col = "#1565c0")
  abline(v = 0, lty = 2)
})

# 3. Por episodio: proporcion que paro en la realidad y que el programa manda a parar.
guardar("3_por_episodio.png", 1300, 750, function() {
  e <- aggregate(cbind(real = paro, programa = recomienda_parar) ~ episodio, x, mean)
  e <- e[match(episodios, e$episodio), ]
  op <- par(mar = c(9, 4.5, 3, 1)); on.exit(par(op))
  barplot(t(100 * as.matrix(e[, c("real", "programa")])), beside = TRUE, names.arg = e$episodio,
          las = 2, cex.names = 0.7, col = c("grey40", "#1565c0"), ylim = c(0, 110),
          ylab = "% de los pilotos medidos", main = "Por safety car: pilotos que pararon y que el programa manda a parar")
  legend("topright", bty = "n", fill = c("grey40", "#1565c0"), legend = c("Pararon", "Programa: parar"))
})

# 4. Acuerdo segun el momento de la carrera (10 secciones) y tabla de aciertos.
guardar("4_por_momento.png", 1300, 650, function() {
  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))
  x$sec <- factor(pmin(10, floor(10 * (x$vuelta - 1) / x$vueltas_carrera) + 1), levels = 1:10)
  a <- tapply(x$acuerdo, x$sec, mean); n <- tapply(x$acuerdo, x$sec, length)
  b <- barplot(100 * a, names.arg = paste0(10 * (0:9), "-", 10 * (1:10), "%"), las = 2, cex.names = 0.7,
               ylim = c(0, 110), col = "#1565c0", ylab = "% de acuerdo", main = "Acuerdo por momento del SC")
  text(b, 100 * a, ifelse(is.na(n), "", paste0("n=", n)), pos = 3, cex = 0.7)
  abline(h = 100 * mean(!x$paro), lty = 2)
  mtext("linea: acuerdo de la regla 'nunca parar'", side = 1, line = 3.5, cex = 0.7)
  m <- table(Programa = ifelse(x$recomienda_parar, "parar", "seguir"), Piloto = ifelse(x$paro, "paro", "siguio"))
  mosaicplot(m, color = c("#2e7d32", "#c62828"), main = sprintf("Acuerdo total %.0f %% (n=%d)", 100 * mean(x$acuerdo), nrow(x)))
})
cat("Graficos guardados en", carpeta, "\n")
