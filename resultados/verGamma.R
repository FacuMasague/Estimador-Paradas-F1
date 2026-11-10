x <- read.csv(commandArgs(TRUE)[1]); x <- x[!x$evaluable, ]
cat("GP sin casos:", nrow(x), "\n")
x$total <- -x$gamma * x$vueltas_carrera
x$por_km <- -x$gamma / x$t_med * 100
f <- function(d, n) cat(sprintf("%-8s n=%2d gamma med %+.3f (cuart %+.3f a %+.3f) | gamma*N med %.1f (cuart %.1f a %.1f) | gamma/t %% %.3f\n", n, nrow(d), median(d$gamma), quantile(d$gamma,.25), quantile(d$gamma,.75), median(d$total), quantile(d$total,.25), quantile(d$total,.75), median(d$por_km)))
f(x, "todos"); f(x[x$temporada < 2026,], "23-25"); f(x[x$temporada >= 2026,], "2026")
cat("Spearman gamma ~ 1/N:", round(cor(x$gamma, 1/x$vueltas_carrera, method="spearman"),2), " gamma ~ t_med:", round(cor(x$gamma, x$t_med, method="spearman"),2), "\n")
e <- function(p) median(abs(x$gamma - p))
cat("Error mediano: constante", round(e(median(x$gamma)),4), "| c/N", round(e(-median(x$total)/x$vueltas_carrera),4), "| k*t_med", round(e(-median(x$por_km)/100*x$t_med),4), "\n")
