# Largo maximo de los stints en carrera, en vueltas y en desgaste acumulado. SOLO LECTURA.
v <- read.csv("resultados/vueltas_carrera_limpias.csv")
st <- aggregate(edad ~ meeting_key + temporada + driver_number + stint + compound + N, v, max)
deg <- do.call(rbind, lapply(split(v, v$meeting_key), function(x) {
  x$compound <- factor(x$compound); x$piloto <- factor(x$driver_number)
  if (nlevels(x$compound) < 2) return(NULL)
  co <- coef(lm(t ~ piloto + compound + compound:edad + lap_number, x)); tm <- median(x$t)
  data.frame(meeting_key = x$meeting_key[1], compound = levels(x$compound),
             deg = sapply(levels(x$compound), function(k) 100 * co[[paste0("compound", k, ":edad")]] / tm))
}))
st <- merge(st, deg, by = c("meeting_key", "compound"))
st$vida <- st$edad + 1
st$desgaste <- st$deg * st$vida
st <- st[st$vida >= 5 & is.finite(st$deg), ]
cat("Stints:", nrow(st), "\n")
for (k in c("SOFT", "MEDIUM", "HARD")) { d <- st[st$compound == k, ]
  cat(sprintf("%-6s n=%4d | vueltas p50 %2.0f p90 %2.0f p95 %2.0f | desgaste %% p50 %.2f p90 %.2f p95 %.2f | variacion entre GP de p90 en vueltas (MAD) %.1f\n",
    k, nrow(d), median(d$vida), quantile(d$vida, .9), quantile(d$vida, .95), median(d$desgaste[d$deg > 0]), quantile(d$desgaste[d$deg > 0], .9), quantile(d$desgaste[d$deg > 0], .95),
    mad(tapply(d$vida, d$meeting_key, quantile, .9)))) }
# Que forma explica mejor la vida maxima de cada GP: constante en vueltas o en desgaste?
g <- aggregate(cbind(vida, desgaste) ~ meeting_key + compound + deg + N, st[st$deg > 0.01, ], function(z) quantile(z, .9))
for (k in c("SOFT", "MEDIUM", "HARD")) { d <- g[g$compound == k, ]
  cat(sprintf("%-6s GP=%2d | CV vida en vueltas %.2f | CV en desgaste %.2f | spearman vida~1/deg %.2f | vida~N %.2f\n", k, nrow(d),
    sd(d$vida) / mean(d$vida), sd(d$desgaste) / mean(d$desgaste), cor(d$vida, 1 / d$deg, method = "spearman"), cor(d$vida, d$N, method = "spearman"))) }
