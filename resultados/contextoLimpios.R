# Contexto de carrera de los 10 casos limpios. SOLO LECTURA.
invisible(capture.output(source("estimadorR.R")))
x <- read.csv("resultados/casos_limpios.csv"); t <- read.csv("resultados/trafico.csv")
con <- dbConnect(SQLite(), rutaBase, flags = SQLITE_RO)
for (i in seq_len(nrow(x))) {
  k <- x[i, ]; dors <- t$dorsal[t$piloto == k$piloto & t$gp == k$gp & t$temporada == k$temporada]
  gp <- gpConTramosCandidatos[gpConTramosCandidatos$temporada == k$temporada & gpConTramosCandidatos$nombre == k$gp, ]
  car <- leerCarreraGP(rutaBase, gp$meeting_key); N <- ultimaVueltaReal(car$vueltas)
  # Orden de llegada aproximado y ritmo mediano de vueltas limpias frente al resto.
  ritmo <- sapply(unique(car$vueltas$driver_number), function(d) { v <- vueltasCarreraPiloto(car, d); median(v$lap_duration[v$limpia_carrera], na.rm = TRUE) })
  names(ritmo) <- unique(car$vueltas$driver_number)
  pos <- reconstruirPosiciones(rutaBase); pos <- pos[pos$fecha == car$sesion$fecha_inicio, ]
  cat(sprintf("\n=== %d %s - %s (#%d) | llegada aprox. P%d | ritmo mediano #%d de %d (%.2f s vs mediana %.2f)\n", k$temporada, k$gp, k$piloto, dors,
    pos$posicion[pos$driver_number == dors], rank(ritmo)[as.character(dors)], sum(is.finite(ritmo)), ritmo[as.character(dors)], median(ritmo, na.rm = TRUE)))
  cat(sprintf("Real %s (%s) | modelo %s (%s) | real - simulada %.1f s | perdida boxes modelo %.1f real %.1f\n", k$real_estr, k$real_stints, k$opt_estr, k$opt_stints, k$real_menos_sim, k$perdida_modelo, k$perdida_real))
  b <- dbGetQuery(con, "SELECT lap_number, pit_duration, stop_duration, lane_duration FROM boxes WHERE session_key = ? AND driver_number = ?", list(car$sesion$session_key, dors))
  if (nrow(b)) print(b, row.names = FALSE)
  m <- car$control[grepl(paste0("CAR ", dors, " ", "|CAR ", dors, "\\)|CAR ", dors, "$"), car$control$message), c("lap_number", "message")]
  m <- m[!grepl("TRACK LIMITS|BLUE FLAG", m$message), ]
  if (nrow(m)) print(head(m, 8), row.names = FALSE) else cat("Sin mensajes de direccion de carrera relevantes para el auto.\n")
}
dbDisconnect(con)
