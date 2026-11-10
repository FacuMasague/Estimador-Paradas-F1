# Validacion de la decision ante VSC/SC contra lo que hicieron los pilotos en carreras
# reales secas con neutralizaciones (y practicas utilizables). Para cada episodio y piloto
# con modelo: estado real al empezar el episodio (compuesto, edad, paradas, compuestos
# usados) -> recomendacion del programa; se compara con si el piloto entro a boxes durante
# el episodio. SOLO LECTURA. Uso desde el proyecto: Rscript resultados/validarNeutralizaciones.R [parte partes]
a <- commandArgs(TRUE)
invisible(capture.output(source("estimadorR.R")))
cfg <- parametros$neutralizacion
gps <- gpConTramosCandidatos
if (length(a) >= 2) gps <- gps[seq_len(nrow(gps)) %% as.integer(a[2]) == as.integer(a[1]) - 1, ]
filas <- list()
for (g in seq_len(nrow(gps))) {
  gp <- gps[g, , drop = FALSE]
  car <- leerCarreraGP(rutaBase, gp$meeting_key)
  if (!isTRUE(car$verificada) || nrow(car$vueltas) == 0) next
  f <- aFechaUTC(car$clima$date)
  ll <- car$clima$rainfall[f >= aFechaUTC(car$sesion$fecha_inicio) & f <= aFechaUTC(car$sesion$fecha_fin)]
  if (length(ll) == 0 || !all(ll %in% 0) || any(car$stints$compound %in% c("INTERMEDIATE", "WET"))) next
  ep <- episodiosNeutralizacion(car$control)
  N <- ultimaVueltaReal(car$vueltas)
  ep <- ep[ep$tipo %in% cfg$tipos & ep$cierre %in% c("mensaje_de_fin", "conversion_a_SC") & is.finite(ep$vuelta_inicio) & ep$vuelta_inicio <= N - 3, ]
  if (nrow(ep) == 0) next
  tramos <- revisionClima[revisionClima$meeting_key %in% gp$meeting_key & revisionClima$estado %in% c("candidato_completo", "candidato_parcial"), ]
  pil <- suppressWarnings(leerPilotosPracticas(rutaBase, tramos$session_key))
  campo <- tryCatch(campoGP(gp, pil$driver_number, tramos$session_key, revisionClima, rutaBase, parametros), error = function(e) NULL)
  if (is.null(campo)) next
  perdida <- estimarPerdidaBoxes(rutaBase, gp$meeting_key)$valor
  if (!is.finite(perdida)) next
  for (d in pil$driver_number) {
    modelo <- modeloDePiloto(d, campo, parametros)
    if (!all(modelo$suficiente)) next
    st <- car$stints[car$stints$driver_number %in% d & car$stints$compound %in% compuestosSecos, ]
    st <- st[order(st$stint_number), ]
    if (nrow(st) == 0 || any(!is.finite(c(st$lap_start, st$lap_end, st$tyre_age_at_start)))) next
    for (k in seq_len(nrow(ep))) {
      L <- ep$vuelta_inicio[k]; fin <- L + max(ep$vueltas[k], 1) - 1
      j <- which(st$lap_start <= L & st$lap_end >= L)
      if (length(j) != 1) next
      if (st$lap_end[nrow(st)] < N - 1) next             # abandono: no se sabe que hubiera hecho
      if (st$lap_end[j] == L - 1 || st$lap_start[j] > L - 1 && j > 1) next  # recien paro
      edad <- st$tyre_age_at_start[j] + (L + 1 - st$lap_start[j])
      usados <- unique(st$compound[seq_len(j)])
      # Estado real: se reemplaza con el del piloto (compuesto, edad, paradas, usados).
      e <- list(R = N - L, actual = st$compound[j], edad = edad, hechas = j - 1, usados = usados,
                modelo = modelo, perdidaVerde = perdida, maxParadas = cfg$maxParadas,
                minVueltasStint = parametros$minVueltasStint, edadInicial = parametros$edadInicial,
                maxFactorEdad = parametros$maxFactorEdad, vidaMaxima = parametros$vidaMaxima)
      seguir <- do.call(mejorContinuacion, c(e, parar = FALSE))
      parar <- do.call(mejorContinuacion, c(e, parar = TRUE, perdidaAhora = cfg$factorPerdida[[ep$tipo[k]]] * perdida))
      ahorro <- if (!is.finite(parar$tiempo)) NA else if (!is.finite(seguir$tiempo)) Inf else seguir$tiempo - parar$tiempo
      paro <- any(st$lap_end >= L & st$lap_end <= fin & seq_len(nrow(st)) < nrow(st))
      filas[[length(filas) + 1]] <- data.frame(temporada = gp$temporada, gp = gp$nombre, tipo = ep$tipo[k], vuelta = L,
        vueltas_carrera = N, dorsal = d, compuesto = st$compound[j], edad = edad, paradas_hechas = j - 1,
        ahorro = ahorro, recomienda_parar = isTRUE(ahorro > 0), paro = paro,
        siguiente_modelo = if (isTRUE(ahorro > 0)) parar$siguiente else NA,
        siguiente_real = if (paro) st$compound[which(st$lap_end >= L & st$lap_end <= fin)[1] + 1] else NA,
        vuelta_parada_real = if (paro) st$lap_end[which(st$lap_end >= L & st$lap_end <= fin)[1]] else NA,
        fin_episodio = fin, perdida = perdida,
        paradas_finales = nrow(st) - 1, termino = st$lap_end[nrow(st)] >= N - 1)
    }
  }
  cat(gp$temporada, gp$nombre, nrow(ep), "episodios\n")
}
x <- do.call(rbind, filas)
write.csv(x, if (length(a) >= 2) sprintf("resultados/validacion_neutralizaciones_%s.csv", a[1]) else "resultados/validacion_neutralizaciones.csv", row.names = FALSE)
