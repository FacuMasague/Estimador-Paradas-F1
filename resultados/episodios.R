# Episodios de VSC y SC de todas las carreras de la base. SOLO LECTURA.
# Uso desde el proyecto: Rscript resultados/episodios.R
invisible(capture.output(source("estimadorR.R")))
carreras <- sesiones[sesiones$nombre == "Race" & sesiones$presente_en_fuente %in% 1, ]
filas <- list(); resumen <- list()
for (k in seq_len(nrow(carreras))) {
  m <- carreras$meeting_key[k]
  car <- leerCarreraGP(rutaBase, m)
  if (!isTRUE(car$verificada) || nrow(car$vueltas) == 0) next
  N <- ultimaVueltaReal(car$vueltas)
  f <- aFechaUTC(car$clima$date)
  ll <- car$clima$rainfall[f >= aFechaUTC(car$sesion$fecha_inicio) & f <= aFechaUTC(car$sesion$fecha_fin)]
  seca <- length(ll) > 0 && all(ll %in% 0) && !any(car$stints$compound %in% c("INTERMEDIATE", "WET"))
  info <- grandesPremios[grandesPremios$meeting_key == m, ]
  e <- episodiosNeutralizacion(car$control)
  roja <- any(car$control$flag %in% "RED")
  sprint <- any(sesiones$meeting_key == m & sesiones$nombre == "Sprint")
  resumen[[length(resumen) + 1]] <- data.frame(meeting_key = m, temporada = info$temporada, gp = info$nombre,
    circuito_key = info$circuito_key, circuito = info$circuito, fecha = car$sesion$fecha_inicio, vueltas_carrera = N,
    seca = seca, roja = roja, sprint = sprint, n_vsc = sum(e$tipo == "VSC"), n_sc = sum(e$tipo == "SC"))
  if (nrow(e)) filas[[length(filas) + 1]] <- data.frame(meeting_key = m, temporada = info$temporada, gp = info$nombre,
    circuito_key = info$circuito_key, vueltas_carrera = N, seca = seca, e)
}
r <- do.call(rbind, resumen); e <- do.call(rbind, filas)
write.csv(r, "resultados/carreras_neutralizaciones.csv", row.names = FALSE)
write.csv(e, "resultados/episodios_neutralizacion.csv", row.names = FALSE)
cat("Carreras:", nrow(r), " secas:", sum(r$seca), " con roja:", sum(r$roja), "\n")
cat("Episodios:\n"); print(table(e$tipo, e$cierre))
cat("Duracion en vueltas (cerrados, no convertidos):\n")
d <- e[e$cierre == "mensaje_de_fin", ]; print(t(sapply(split(d$vueltas, d$tipo), quantile, c(0, .25, .5, .75, 1))))
cat("Duracion en segundos:\n"); print(t(sapply(split(d$segundos, d$tipo), quantile, c(.25, .5, .75))))
cat("Vueltas de inicio invalidas:", sum(!is.finite(e$vuelta_inicio)), "\n")
