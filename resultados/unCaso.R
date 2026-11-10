a <- commandArgs(TRUE)
invisible(capture.output(source("estimadorR.R")))
gp <- gpConTramosCandidatos[gpConTramosCandidatos$temporada == as.numeric(a[1]) & gpConTramosCandidatos$nombre == a[2], , drop = FALSE]
tramos <- revisionClima[revisionClima$meeting_key %in% gp$meeting_key & revisionClima$estado %in% c("candidato_completo", "candidato_parcial"), ]
pil <- leerPilotosPracticas(rutaBase, tramos$session_key)
sel <- list(temporada = gp$temporada, granPremio = gp, piloto = pil[pil$driver_number == as.numeric(a[3]), , drop = FALSE], tramos = tramos)
t0 <- Sys.time(); r <- analizarCaso(sel, a[4], revisionClima, rutaBase, parametros); print(Sys.time() - t0)
