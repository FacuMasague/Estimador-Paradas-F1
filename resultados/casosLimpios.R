# Tiempo de carrera real, real simulada y optima simulada en los casos de aire limpio.
# Uso: Rscript casosLimpios.R <proyecto> <trafico.csv> <salida.csv>
a <- commandArgs(TRUE); setwd(a[1])
invisible(capture.output(source("estimadorR.R")))
t <- read.csv(a[2])
sel <- t[t$vueltas_piloto == t$vueltas_carrera & t$pct_aire_limpio >= 75 & t$cambios_pista <= 1, ]
filas <- list()
for (i in seq_len(nrow(sel))) {
  k <- sel[i, ]
  gp <- gpConTramosCandidatos[gpConTramosCandidatos$temporada == k$temporada & gpConTramosCandidatos$nombre == k$gp, , drop = FALSE]
  tramos <- revisionClima[revisionClima$meeting_key %in% gp$meeting_key & revisionClima$estado %in% c("candidato_completo", "candidato_parcial"), ]
  pil <- suppressWarnings(leerPilotosPracticas(rutaBase, tramos$session_key))
  s <- list(temporada = k$temporada, granPremio = gp, piloto = pil[pil$driver_number == k$dorsal, , drop = FALSE], tramos = tramos)
  invisible(capture.output(r <- analizarCaso(s, "investigacion", revisionClima, rutaBase, parametros)))
  N <- r$nVueltas
  carrera <- leerCarreraGP(rutaBase, gp$meeting_key)
  v <- vueltasCarreraPiloto(carrera, k$dorsal)
  v <- v[v$lap_number >= 2 & v$lap_number <= N, ]
  real <- sum(v$lap_duration)                      # vueltas 2 a N, con boxes incluidos
  stRec <- data.frame(compound = strsplit(r$mejores$secuencia[1], "-")[[1]])
  largos <- diff(c(0, as.numeric(strsplit(r$mejores$vueltas_parada[1], "-")[[1]]), N))
  stRec$lap_end <- cumsum(largos); stRec$lap_start <- stRec$lap_end - largos + 1
  stRec$tyre_age_at_start <- parametros$edadInicial[stRec$compound]
  total <- function(st, ed) {
    st$tyre_age_at_start <- ed
    p <- predecirVueltas(st, r$modelo, r$combustible, N)
    sum(p$prediccion[p$lap_number >= 2]) + (nrow(st) - 1) * r$perdida
  }
  simReal <- total(r$real$stints, r$real$stints$tyre_age_at_start)
  simRealEsp <- total(r$real$stints, parametros$edadInicial[r$real$stints$compound])
  simOpt <- total(stRec, stRec$tyre_age_at_start)
  # Perdida real por parada del piloto: entrada + salida - 2 x mediana de vueltas limpias.
  ref <- median(v$lap_duration[v$limpia_carrera])
  perdidaReal <- mean(sapply(r$real$vueltas_parada, function(L)
    sum(v$lap_duration[v$lap_number %in% c(L, L + 1)]) - 2 * ref))
  # Tiempo real con vueltas 2 a N y duraciones faltantes
  filas[[i]] <- data.frame(temporada = k$temporada, gp = k$gp, piloto = k$piloto,
    aire = round(k$pct_aire_limpio), cambios = k$cambios_pista, vueltas_sumadas = sum(is.finite(v$lap_duration)),
    faltan = N - 1 - sum(is.finite(v$lap_duration)),
    real_estr = paste(r$real$stints$compound, collapse = "-"), real_stints = paste(diff(c(0, r$real$vueltas_parada, N)), collapse = "+"),
    opt_estr = r$mejores$secuencia[1], opt_stints = r$mejores$stints[1],
    real_s = round(real, 1), sim_real_s = round(simReal, 1), sim_real_esp_s = round(simRealEsp, 1), sim_opt_s = round(simOpt, 1),
    real_menos_sim = round(real - simReal, 1), sim_menos_opt = round(simRealEsp - simOpt, 1),
    margen = round(r$margen$margen, 1), prob_real_mejor = r$margen$prob_real_mejor,
    mae_aj = round(r$comparacion$mae_ajustado, 2), desplaz = round(r$comparacion$desplazamiento, 2),
    perdida_modelo = round(r$perdida, 1), perdida_real = round(perdidaReal, 1), paradas = r$real$paradas)
  cat(k$temporada, k$gp, k$piloto, "\n")
}
x <- do.call(rbind, filas)
write.csv(x, a[3], row.names = FALSE)
