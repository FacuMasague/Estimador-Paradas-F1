x <- read.csv("resultados/error_nivel2.csv"); x$clave <- paste(x$temporada, x$gp)
g <- aggregate(cbind(pct, desp, deg_tipica) ~ clave + temporada + evaluable + sprint, x, median)
g$n <- as.numeric(table(x$clave)[g$clave])
cal <- g[!g$evaluable, ]; ev <- g[g$evaluable, ]
cat("GP sin casos:", nrow(cal), "(Sprint", sum(cal$sprint), ") | piloto-GP", sum(cal$n), "| evaluables", nrow(ev), "\n")
ic <- function(v) { set.seed(1); b <- replicate(5000, median(sample(v, replace = TRUE))); sprintf("n=%2d mediana %+.2f %% IC %+.2f a %+.2f | negativos %d", length(v), median(v), quantile(b,.025), quantile(b,.975), sum(v < 0)) }
cat("Todos sin casos:  ", ic(cal$pct), "\n"); cat("Tres practicas:   ", ic(cal$pct[!cal$sprint]), "\n"); cat("Sprint:           ", ic(cal$pct[cal$sprint]), "\n")
cat("Sin deg tipica:   ", ic(cal$pct[cal$deg_tipica == 0]), "\n"); cat("Evaluables:       ", ic(ev$pct), "\n")
for (t in sort(unique(cal$temporada))) cat(t, ic(cal$pct[cal$temporada == t]), "\n")
d <- x[!x$evaluable, ]; cat("Segundos: mediana entre GP", round(median(cal$desp), 2), "\n")
k <- median(cal$pct)
xe <- x[x$evaluable, ]
cat(sprintf("Evaluables por piloto, error abs: sin corregir %.2f | con %.2f %%: %.2f | resto %+.2f s\n", median(abs(xe$desp)), k, median(abs(xe$desp - k/100*xe$t_med)), median(xe$desp - k/100*xe$t_med)))
cat(sprintf("Sin casos por piloto: sin corregir %.2f | con: %.2f\n", median(abs(d$desp)), median(abs(d$desp - k/100*d$t_med))))
