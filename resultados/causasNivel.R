library(DBI); library(RSQLite)
x <- read.csv("resultados/error_nivel.csv")
x$clave <- paste(x$temporada, x$gp)
g <- aggregate(pct ~ clave + temporada + gp + evaluable, x, median)
cal <- g[!g$evaluable, ]
set.seed(1)
b <- replicate(5000, median(sample(cal$pct, replace = TRUE)))
cat(sprintf("GP sin casos n=%d: mediana %+.2f %%, IC 95 %% bootstrap %+.2f a %+.2f\n", nrow(cal), median(cal$pct), quantile(b, .025), quantile(b, .975)))
b2 <- replicate(5000, median(sample(g$pct, replace = TRUE)))
cat(sprintf("Todos los GP n=%d (solo para comparar): mediana %+.2f %%, IC %+.2f a %+.2f\n", nrow(g), median(g$pct), quantile(b2, .025), quantile(b2, .975)))
cat("GP con error negativo:", sum(cal$pct < 0), "de", nrow(cal), "\n")
con <- dbConnect(SQLite(), "datosOpenF1.sqlite3", flags = SQLITE_RO)
cl <- dbGetQuery(con, "SELECT g.temporada, g.nombre gp, s.nombre sesion, avg(c.track_temperature) pista, avg(c.air_temperature) aire
  FROM clima c JOIN sesiones s ON s.session_key = c.session_key JOIN grandes_premios g ON g.meeting_key = s.meeting_key
  WHERE s.nombre IN ('Race','Practice 1','Practice 2','Practice 3') GROUP BY 1,2,3")
dbDisconnect(con)
r <- cl[cl$sesion == "Race", ]; p <- aggregate(cbind(pista, aire) ~ temporada + gp, cl[cl$sesion != "Race", ], mean)
m <- merge(merge(g, r[, c("temporada","gp","pista","aire")], by = c("temporada","gp")), p, by = c("temporada","gp"), suffixes = c("_c","_p"))
m$dif_pista <- m$pista_c - m$pista_p
cat(sprintf("Pista carrera - practicas: mediana %+.1f C, cuartiles %+.1f a %+.1f (n=%d)\n", median(m$dif_pista), quantile(m$dif_pista,.25), quantile(m$dif_pista,.75), nrow(m)))
cat("Spearman error ~ dif pista:", round(cor(m$pct, m$dif_pista, method = "spearman"), 2), " p =", round(cor.test(m$pct, m$dif_pista, method = "spearman", exact = FALSE)$p.value, 3), "\n")
k <- m[abs(m$dif_pista) >= 0, ]
print(aggregate(pct ~ cut(dif_pista, c(-Inf, -5, 5, Inf), labels = c("carrera mas fria (>5C)", "similar", "carrera mas caliente (>5C)")), m, function(v) round(c(n = length(v), mediana = median(v)), 2)))
