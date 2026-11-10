x <- do.call(rbind, lapply(Sys.glob("resultados/validacion_neutralizaciones_*.csv"), read.csv))
write.csv(x, "resultados/validacion_neutralizaciones.csv", row.names = FALSE)
cat("Decisiones:", nrow(x), " GP:", length(unique(paste(x$temporada, x$gp))), " episodios:", nrow(unique(x[, c("temporada","gp","tipo","vuelta")])), "\n")
x <- x[!is.na(x$ahorro), ]
cat("Con parada posible:", nrow(x), "\n")
r <- function(d, n) cat(sprintf("%-28s n=%4d | pararon %3.0f%% | modelo dice parar %3.0f%% | acuerdo %3.0f%% | acuerdo si siempre 'seguir' %3.0f%% | si siempre 'parar' %3.0f%%\n",
  n, nrow(d), 100*mean(d$paro), 100*mean(d$recomienda_parar), 100*mean(d$paro == d$recomienda_parar), 100*mean(!d$paro), 100*mean(d$paro)))
r(x, "Todas"); for (t in c("SC","VSC")) r(x[x$tipo == t, ], t)
cat("\nMatriz (filas: modelo, columnas: real):\n"); print(table(modelo = ifelse(x$recomienda_parar, "parar", "seguir"), real = ifelse(x$paro, "paro", "siguio"), tipo = x$tipo))
x$banda <- cut(pmin(pmax(x$ahorro, -60), 60), c(-61, -10, -3, 0, 3, 10, 61), labels = c("< -10", "-10 a -3", "-3 a 0", "0 a 3", "3 a 10", "> 10"))
cat("\nProporcion que paro segun el ahorro estimado de parar (s):\n")
print(aggregate(cbind(pararon = paro) ~ banda + tipo, x, function(v) round(c(n = length(v), pct = 100*mean(v)), 0)))
fuerte <- abs(x$ahorro) >= 5
r(x[fuerte, ], "Con |ahorro| >= 5 s")
cat("\nCompuesto siguiente cuando ambos paran:\n"); y <- x[x$paro & x$recomienda_parar, ]; cat("coincide", sum(y$siguiente_modelo == y$siguiente_real, na.rm=TRUE), "de", nrow(y), "\n")
print(table(modelo = y$siguiente_modelo, real = y$siguiente_real))
# Por momento de la carrera
x$sec <- pmin(10, floor(10 * (x$vuelta - 1) / x$vueltas_carrera) + 1)
cat("\nPor seccion de la carrera (todas):\n"); print(aggregate(cbind(paro, recomienda_parar, acuerdo = paro == recomienda_parar) ~ sec, x, function(v) round(100*mean(v))))
# Por episodio: mayoria del campo
e <- aggregate(cbind(paro, recomienda_parar) ~ temporada + gp + tipo + vuelta, x, mean)
cat("\nPor episodio (proporcion del campo): correlacion modelo-real", round(cor(e$paro, e$recomienda_parar), 2), " n", nrow(e), "\n")
cat("Episodios donde la mayoria paro:", sum(e$paro > .5), " y el modelo dice parar a la mayoria:", sum(e$recomienda_parar > .5), " coinciden en", sum((e$paro > .5) == (e$recomienda_parar > .5)), "\n")
