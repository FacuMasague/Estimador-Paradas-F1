v <- c("1. Calibracion inicial" = "resultados/casos_limpios_v1.csv",
       "2. Combustible y degradacion corregidos" = "resultados/casos_limpios_v3_sin_nivel.csv",
       "3. + nivel -0,98 %" = "resultados/casos_limpios_v4_sin_vida.csv",
       "4. Todos los GP + limite de vida" = "resultados/casos_limpios.csv")
comb <- function(x) sapply(strsplit(x, "-"), function(z) paste(sort(unique(z)), collapse = "+"))
par <- function(x) lengths(strsplit(x, "+", fixed = TRUE)) - 1
for (n in names(v)) {
  x <- read.csv(v[[n]])
  cat(sprintf("%-42s |real-sim| %5.1f s (%.2f %%) | forma %.2f s | mismas paradas %d/10 | mismos comp %d/10 | opt-real %4.1f s | margen %5.1f\n", n,
    median(abs(x$real_menos_sim)), 100 * median(abs(x$real_menos_sim) / x$real_s), median(x$mae_aj),
    sum(par(x$real_stints) == par(x$opt_stints)), sum(comb(x$real_estr) == comb(x$opt_estr)),
    median(x$sim_menos_opt), median(x$margen)))
}
x <- read.csv(v[[4]]); y <- read.csv(v[[1]])
print(data.frame(piloto = paste(x$piloto, x$gp), antes = y$real_menos_sim, ahora = x$real_menos_sim, rec_antes = y$opt_estr, rec_ahora = x$opt_estr, real = x$real_estr), row.names = FALSE)
