a <- commandArgs(TRUE); setwd(a[1])
invisible(capture.output(source("estimadorR.R")))
casos <- read.csv("resultados/investigacion_casos.csv")
filas <- list()
for (clave in unique(paste(casos$temporada, casos$gp))) {
  ca <- casos[paste(casos$temporada, casos$gp) == clave, ]
  gp <- gpConTramosCandidatos[gpConTramosCandidatos$temporada == ca$temporada[1] & gpConTramosCandidatos$nombre == ca$gp[1], ]
  carrera <- leerCarreraGP(rutaBase, gp$meeting_key)
  N <- ultimaVueltaReal(carrera$vueltas)
  kg <- if (gp$temporada >= 2026) 70 else 105
  fuel <- 3.1 / N
  for (i in seq_len(nrow(ca))) {
    v <- vueltasCarreraPiloto(carrera, ca$dorsal[i])
    v <- v[v$limpia_carrera & v$compound %in% compuestosSecos & v$lap_number <= N, ]
    for (st in unique(v$stint_number)) {
      x <- v[v$stint_number == st, ]
      if (nrow(x) < 8 || diff(range(x$lap_number)) < 5) next
      x <- x[x$lap_duration <= 1.05 * median(x$lap_duration), ]
      real <- ajustarTheilSen(x$lap_number, x$lap_duration)$pendiente
      k <- substr(x$compound[1], 1, 1)
      prev <- ca[[paste0("deg_", k)]][i] / 100 * median(x$lap_duration)
      filas[[length(filas) + 1]] <- data.frame(gp = clave, compuesto = x$compound[1], real_neta = real,
        prev_deg = max(prev, 0), fuel = fuel, largo = nrow(x))
    }
  }
}
x <- do.call(rbind, filas)
x$prev_neta <- x$prev_deg - x$fuel; x$real_deg <- x$real_neta + x$fuel
cat("Stints:", nrow(x), "\n")
for (c in c("SOFT","MEDIUM","HARD","TODOS")) {
  d <- if (c == "TODOS") x else x[x$compuesto == c, ]
  cat(sprintf("%-7s n=%3d | neta real %+.3f (baja en %2.0f%%) | neta prevista %+.3f (baja en %2.0f%%) | deg real %.3f | deg usada %.3f | combustible %.3f\n",
    c, nrow(d), median(d$real_neta), 100*mean(d$real_neta < 0), median(d$prev_neta), 100*mean(d$prev_neta < 0), median(d$real_deg), median(d$prev_deg), median(d$fuel)))
}
