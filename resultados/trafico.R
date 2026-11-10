# Trafico por piloto en los GP elegibles, desde los cruces de meta. SOLO LECTURA.
# Uso: Rscript trafico.R <proyecto> <casos.csv> <salida.csv>
a <- commandArgs(TRUE); setwd(a[1])
invisible(capture.output(source("estimadorR.R")))
casos <- read.csv(a[2])
filas <- list()
for (clave in unique(paste(casos$temporada, casos$gp))) {
  ca <- casos[paste(casos$temporada, casos$gp) == clave, ]
  gp <- gpConTramosCandidatos[gpConTramosCandidatos$temporada == ca$temporada[1] & gpConTramosCandidatos$nombre == ca$gp[1], ]
  carrera <- leerCarreraGP(rutaBase, gp$meeting_key)
  ult <- ultimaVueltaReal(carrera$vueltas)
  v <- carrera$vueltas[carrera$vueltas$lap_number <= ult, c("driver_number","lap_number","date_start","lap_duration","is_pit_out_lap")]
  v$t <- as.numeric(aFechaUTC(v$date_start))
  v <- v[is.finite(v$t), ]
  sal <- paste(v$driver_number, v$lap_number)[v$is_pit_out_lap %in% 1]
  # Vueltas de boxes de cada piloto: la de salida y la anterior (entrada).
  enBoxes <- unique(c(sal, sapply(strsplit(sal, " "), function(z) paste(z[1], as.numeric(z[2]) - 1))))
  for (d in ca$dorsal) {
    lim <- vueltasCarreraPiloto(carrera, d); lim <- lim[lim$limpia_carrera, ]
    mias <- v[v$driver_number == d & v$lap_number %in% lim$lap_number, ]
    otros <- v[v$driver_number != d, ]
    # Hueco al auto fisicamente adelante: tiempo desde el ultimo cruce de cualquier otro.
    hueco <- sapply(mias$t, function(t) { p <- otros$t[otros$t < t]; if (length(p)) t - max(p) else NA })
    hueco <- hueco[is.finite(hueco)]
    # Cambios de orden en pista contra cada rival: sin boxes de ninguno de los dos.
    cambios <- 0
    yo <- v[v$driver_number == d, ]
    for (o in setdiff(unique(v$driver_number), d)) {
      x <- merge(yo[, c("lap_number","t")], v[v$driver_number == o, c("lap_number","t")], by = "lap_number")
      x <- x[order(x$lap_number), ]
      x <- x[!paste(d, x$lap_number) %in% enBoxes & !paste(o, x$lap_number) %in% enBoxes & x$lap_number >= 2, ]
      s <- sign(x$t.x - x$t.y)
      if (length(s) > 1) cambios <- cambios + sum(s[-1] != s[-length(s)] & diff(x$lap_number) == 1)
    }
    filas[[length(filas) + 1]] <- data.frame(temporada = ca$temporada[1], gp = ca$gp[1], dorsal = d,
      piloto = ca$piloto[ca$dorsal == d], vueltas_limpias = length(hueco),
      pct_aire_limpio = 100 * mean(hueco >= 2), pct_drs = 100 * mean(hueco < 1),
      hueco_mediano = median(hueco), cambios_pista = cambios,
      vueltas_piloto = max(yo$lap_number), vueltas_carrera = ult)
  }
  cat(clave, "\n")
}
x <- do.call(rbind, filas)
write.csv(x, a[3], row.names = FALSE)
