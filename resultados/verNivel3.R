x <- read.csv("resultados/error_nivel2.csv"); x$clave <- paste(x$temporada, x$gp)
g <- aggregate(pct ~ clave + evaluable + sprint, x, median)
for (s in 1:3) { set.seed(s*100); for (n in c("todos","tres")) { v <- g$pct[!g$evaluable & (n == "todos" | !g$sprint)]; b <- replicate(20000, median(sample(v, replace = TRUE))); cat(sprintf("semilla %d %-5s n=%d %+.2f IC %+.2f a %+.2f\n", s, n, length(v), median(v), quantile(b,.025), quantile(b,.975))) } }
print(g[g$evaluable & g$sprint, ])
e <- x[x$evaluable & !x$sprint, ]
for (k in c(0, -0.98, -1.27)) cat(sprintf("Casos (3 practicas), k=%+.2f: error abs %.2f, resto mediano %+.2f s\n", k, median(abs(e$desp - k/100*e$t_med)), median(e$desp - k/100*e$t_med)))
z <- read.csv("resultados/casos_limpios.csv"); for (k in c(0.0098, 0.0127)) { d <- z$real_s - (z$sim_real_s - k*(z$sim_real_s - z$paradas*z$perdida_modelo)); cat(sprintf("Limpios k=%.2f %%: mediana %.0f s, %.2f %%\n", 100*k, median(d), 100*median(abs(d)/z$real_s))) }
