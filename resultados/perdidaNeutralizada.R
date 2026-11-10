# Perdida por parada en bandera verde, VSC y SC, medida igual en los tres casos:
# (vuelta de entrada + vuelta de salida del que para) - mediana de las mismas dos
# vueltas de los autos que no pararon y estaban en pista en ese momento. SOLO LECTURA.
# Una parada es bajo VSC/SC si el auto entra a boxes (fin de su vuelta de entrada)
# durante el episodio. Uso desde el proyecto: Rscript resultados/perdidaNeutralizada.R
invisible(capture.output(source("estimadorR.R")))
carreras <- sesiones[sesiones$nombre == "Race" & sesiones$presente_en_fuente %in% 1, ]
filas <- list()
for (m in carreras$meeting_key) {
  car <- leerCarreraGP(rutaBase, m)
  if (!isTRUE(car$verificada) || nrow(car$vueltas) == 0) next
  N <- ultimaVueltaReal(car$vueltas)
  f <- aFechaUTC(car$clima$date)
  ll <- car$clima$rainfall[f >= aFechaUTC(car$sesion$fecha_inicio) & f <= aFechaUTC(car$sesion$fecha_fin)]
  seca <- length(ll) > 0 && all(ll %in% 0) && !any(car$stints$compound %in% c("INTERMEDIATE", "WET"))
  if (!seca) next
  ep <- episodiosNeutralizacion(car$control)
  ep$hasta[is.na(ep$hasta)] <- aFechaUTC(car$sesion$fecha_fin)
  v <- car$vueltas[car$vueltas$lap_number <= N & car$vueltas$lap_number >= 2, ]
  v$t0 <- as.numeric(aFechaUTC(v$date_start)); v$t1 <- v$t0 + v$lap_duration
  v <- v[is.finite(v$t0) & is.finite(v$lap_duration), ]
  nuevos <- car$stints[car$stints$stint_number > 1, ]
  salidas <- paste(nuevos$driver_number, nuevos$lap_start)
  boxes <- unique(c(salidas, paste(nuevos$driver_number, nuevos$lap_start - 1)))
  v$en_boxes <- paste(v$driver_number, v$lap_number) %in% boxes
  # Vueltas que se superponen con algun episodio (para no usarlas como referencia verde).
  v$tipo <- "verde"
  for (k in seq_len(nrow(ep))) {
    a <- as.numeric(ep$desde[k]); b <- as.numeric(ep$hasta[k])
    toca <- v$t0 <= b & v$t1 >= a
    v$tipo[toca & v$tipo == "verde"] <- ep$tipo[k]
    v$tipo[toca & v$tipo != ep$tipo[k] & v$tipo != "verde"] <- "mixta"
  }
  rojas <- any(car$control$flag %in% "RED")
  for (s in salidas) {
    d <- as.integer(strsplit(s, " ")[[1]]); entrada <- v[v$driver_number == d[1] & v$lap_number == d[2] - 1, ]
    salida <- v[v$driver_number == d[1] & v$lap_number == d[2], ]
    if (nrow(entrada) != 1 || nrow(salida) != 1) next
    tEntra <- entrada$t1
    tipo <- "verde"
    for (k in seq_len(nrow(ep))) if (tEntra >= as.numeric(ep$desde[k]) && tEntra <= as.numeric(ep$hasta[k])) tipo <- ep$tipo[k]
    if (tipo == "verde" && (entrada$tipo != "verde" || salida$tipo != "verde")) tipo <- "verde_mezclada"
    # Referencia: para cada otro auto sin boxes, su vuelta que empieza mas cerca del inicio
    # de la vuelta de entrada (a menos de 20 s) y la siguiente.
    ref <- c()
    for (o in setdiff(unique(v$driver_number), d[1])) {
      x <- v[v$driver_number == o, ]; x <- x[order(x$lap_number), ]
      j <- which.min(abs(x$t0 - entrada$t0))
      if (length(j) == 0 || abs(x$t0[j] - entrada$t0) > 20 || j == nrow(x)) next
      if (x$en_boxes[j] || x$en_boxes[j + 1] || x$lap_number[j + 1] != x$lap_number[j] + 1) next
      ref <- c(ref, x$lap_duration[j] + x$lap_duration[j + 1])
    }
    if (length(ref) < 3) next
    filas[[length(filas) + 1]] <- data.frame(meeting_key = m, temporada = car$sesion$fecha_inicio, roja = rojas,
      driver_number = d[1], vuelta = d[2], tipo = tipo, perdida = entrada$lap_duration + salida$lap_duration - median(ref),
      ref_vuelta = median(ref) / 2, autos_ref = length(ref))
  }
}
x <- do.call(rbind, filas)
write.csv(x, "resultados/perdida_neutralizada.csv", row.names = FALSE)
cat("Paradas por tipo:\n"); print(table(x$tipo))
x <- x[x$perdida > 0 & x$perdida < 60, ]
cat("Perdida (s) por tipo, cuantiles 25/50/75:\n"); print(round(t(sapply(split(x$perdida, x$tipo), quantile, c(.25, .5, .75))), 1))
g <- aggregate(perdida ~ meeting_key, x[x$tipo == "verde", ], median); names(g)[2] <- "verde"
for (t in c("VSC", "SC")) {
  y <- merge(aggregate(perdida ~ meeting_key, x[x$tipo == t, ], median), g)
  y$r <- y$perdida / y$verde
  cat(sprintf("%s: carreras %d | proporcion de la perdida verde de esa carrera: mediana %.2f (cuartiles %.2f a %.2f)\n", t, nrow(y), median(y$r), quantile(y$r, .25), quantile(y$r, .75)))
}
