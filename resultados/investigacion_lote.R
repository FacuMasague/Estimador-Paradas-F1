# Corrida por lotes de la version de investigacion: todos los GP evaluables y todos
# los pilotos elegibles. Uso: Rscript investigacion.R <carpeta del proyecto> <salida.csv>
args <- commandArgs(TRUE)
setwd(args[1])
invisible(capture.output(source("estimadorR.R")))   # sin menus (no interactivo)

estados <- sapply(gpConTramosCandidatos$meeting_key, function(m)
  estadoCarreraEvaluable(leerCarreraGP(rutaBase, m)))
gps <- gpConTramosCandidatos[estados == "ok", ]
cat("GP evaluables:", nrow(gps), "\n")
posiciones <- reconstruirPosiciones(rutaBase)

# Opcional: args[3] = parte, args[4] = cantidad de partes (para correr en paralelo).
indices <- seq_len(nrow(gps))
if (length(args) >= 4) indices <- indices[indices %% as.integer(args[4]) == as.integer(args[3]) - 1]
porGP <- lapply(indices, function(g) {
  filas <- list()
  gp <- gps[g, , drop = FALSE]
  t0 <- Sys.time()
  tramos <- revisionClima[revisionClima$meeting_key %in% gp$meeting_key &
                            revisionClima$estado %in% c("candidato_completo", "candidato_parcial"), ]
  pilotos <- leerPilotosPracticas(rutaBase, tramos$session_key)
  elegibles <- tryCatch(pilotosElegiblesGP(gp, pilotos, tramos, "investigacion", revisionClima,
                                           rutaBase, parametros, posiciones),
                        error = function(e) { cat("  error:", conditionMessage(e), "\n"); pilotos[0, ] })
  cat(sprintf("%d %s: %d elegibles de %d\n", gp$temporada, gp$nombre, nrow(elegibles), nrow(pilotos)))
  for (i in seq_len(nrow(elegibles))) {
    sel <- list(temporada = gp$temporada, granPremio = gp, piloto = elegibles[i, , drop = FALSE],
                tramos = tramos)
    r <- NULL
    t0caso <- Sys.time()
    salida <- capture.output(r <- tryCatch(
      analizarCaso(sel, "investigacion", revisionClima, rutaBase, parametros),
      error = function(e) { cat("ERROR", conditionMessage(e)); NULL }))
    if (is.null(r) || is.null(r$comparacion)) {
      cat("  sin resultado para", elegibles$driver_number[i], "\n")
      next
    }
    m <- r$modelo
    recomendadas <- as.numeric(strsplit(r$mejores$vueltas_parada[1], "-")[[1]])
    combinacion <- function(x) paste(sort(unique(strsplit(x, "-")[[1]])), collapse = "-")
    filas[[length(filas) + 1]] <- data.frame(
      temporada = gp$temporada, gp = gp$nombre, dorsal = elegibles$driver_number[i],
      piloto = elegibles$full_name[i], pos_previa = round(elegibles$prioridad[i], 1),
      base_S = m$origen_base[1], base_M = m$origen_base[2], base_H = m$origen_base[3],
      pend_H = m$origen_pendiente[3],
      deg_S = round(m$degradacion_pct[1], 3), deg_M = round(m$degradacion_pct[2], 3),
      deg_H = round(m$degradacion_pct[3], 3),
      real = paste(r$real$stints$compound, collapse = "-"),
      paradas_real = paste(r$real$vueltas_parada, collapse = "-"),
      modelo = r$mejores$secuencia[1], paradas_modelo = r$mejores$vueltas_parada[1],
      mismas_paradas = r$real$paradas == length(recomendadas),
      mismos_compuestos = combinacion(paste(r$real$stints$compound, collapse = "-")) ==
        combinacion(r$mejores$secuencia[1]),
      dif_primera_parada = if (r$real$paradas == length(recomendadas))
        recomendadas[1] - r$real$vueltas_parada[1] else NA,
      real_peor_s = round(r$tiempoReal - r$mejores$tiempo_modelo[1], 1),
      puesto = r$puesto, estrategias = nrow(r$estrategias),
      vueltas = r$comparacion$n, mae = round(r$comparacion$mae_absoluto, 2),
      desplazamiento = round(r$comparacion$desplazamiento, 2),
      mae_ajustado = round(r$comparacion$mae_ajustado, 2),
      perdida_boxes = round(r$perdida, 1), stints_modelo = r$mejores$stints[1],
      stints_real = paste(diff(c(0, r$real$vueltas_parada, r$nVueltas)), collapse = "+"),
      margen = round(r$margen$margen, 1),
      real_en_margen = (r$tiempoReal - r$mejores$tiempo_modelo[1]) <= r$margen$margen,
      combinacion_real_empatada = r$realEmpatada,
      combinaciones_empatadas = length(r$empatadas),
      prob_real_mejor = r$margen$prob_real_mejor,
      p_SC = if (is.null(r$neutralizaciones)) NA else r$neutralizaciones$probabilidades$p[["SC"]],
      ediciones_circuito = if (is.null(r$neutralizaciones)) NA else r$neutralizaciones$probabilidades$ediciones_circuito,
      D = if (is.null(r$neutralizaciones)) NA else r$neutralizaciones$D$esperado,
      D_si_SC = if (is.null(r$neutralizaciones)) NA else r$neutralizaciones$D$condicional[["SC"]],
      secciones_parar_SC = if (is.null(r$neutralizaciones)) NA else sum(r$neutralizaciones$ventanas$secciones$tipo == "SC" & r$neutralizaciones$ventanas$secciones$accion == "parar"),
      respuestas_validas = !is.null(r$neutralizaciones) && nrow(r$neutralizaciones$ventanas$secciones) == 10,
      segundos = as.numeric(difftime(Sys.time(), t0caso, units = "secs")),
      grafico = r$archivoGraficos)
  }
  cat(sprintf("  %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  do.call(rbind, filas)
})
resultado <- do.call(rbind, porGP)
write.csv(resultado, args[2], row.names = FALSE)
cat("Casos:", nrow(resultado), "\n")
