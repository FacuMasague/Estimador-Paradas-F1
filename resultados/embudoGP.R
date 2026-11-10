invisible(capture.output(source("estimadorR.R")))
con <- dbConnect(SQLite(), rutaBase, flags = SQLITE_RO)
ca <- dbGetQuery(con, "SELECT g.temporada, g.nombre, g.meeting_key, s.fecha_inicio, s.cancelada, s.presente_en_fuente,
  (SELECT count(*) FROM vueltas v WHERE v.session_key = s.session_key) vueltas,
  (SELECT count(*) FROM stints t WHERE t.session_key = s.session_key) stints
  FROM sesiones s JOIN grandes_premios g ON g.meeting_key = s.meeting_key WHERE s.nombre = 'Race' ORDER BY s.fecha_inicio")
dbDisconnect(con)
ca$tramos <- ca$meeting_key %in% gpConTramosCandidatos$meeting_key
r <- read.csv("resultados/regresion_carrera.csv"); n <- read.csv("resultados/error_nivel.csv")
ca$en_regresion <- ca$meeting_key %in% r$meeting_key
ca$en_nivel <- paste(ca$temporada, ca$nombre) %in% paste(n$temporada, n$gp)
ca$estado <- sapply(ca$meeting_key, function(m) { c <- leerCarreraGP(rutaBase, m); if (!isTRUE(c$verificada)) return("no_verificada"); f <- aFechaUTC(c$clima$date); l <- c$clima$rainfall[f >= aFechaUTC(c$sesion$fecha_inicio) & f <= aFechaUTC(c$sesion$fecha_fin)]; if (length(l) == 0) "sin_clima" else if (!all(l %in% 0)) "lluvia" else if (any(c$stints$compound %in% c("INTERMEDIATE","WET"))) "neumatico_lluvia" else "seca" })
print(table(ca$temporada, ca$estado))
cat("Total", nrow(ca), "| con vueltas", sum(ca$vueltas > 0), "| con tramos de practica", sum(ca$tramos), "| regresion", sum(ca$en_regresion), "| nivel", sum(ca$en_nivel), "\n")
cat("\nSecas fuera de la regresion:\n"); print(ca[ca$estado == "seca" & !ca$en_regresion, c("temporada","nombre","tramos","vueltas","stints")], row.names = FALSE)
cat("\nEn regresion pero sin nivel:\n"); print(ca[ca$en_regresion & !ca$en_nivel, c("temporada","nombre")], row.names = FALSE)
cat("\nSin vueltas:\n"); print(ca[ca$vueltas == 0, c("temporada","nombre","fecha_inicio","cancelada")], row.names = FALSE)
