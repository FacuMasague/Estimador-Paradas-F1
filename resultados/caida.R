# Caida final del neumatico ("cliff") medida en carrera. SOLO LECTURA.
# Modelo por carrera: t = piloto + compuesto + compuesto:edad + vuelta + c_k * h_k,
# con h_k = max(0, x - umbral_k)^2 para el compuesto k. Dos formas de x:
#  - "edad": x = edad del neumatico (vueltas);
#  - "desgaste": x = degradacion lineal de ese compuesto en esa carrera (% por vuelta) x edad.
# c_k es comun a todas las carreras; el resto de los coeficientes es propio de cada una
# (se eliminan residualizando dentro de cada carrera).
# Uso desde el proyecto: Rscript resultados/caida.R
invisible(capture.output(source("estimadorR.R")))
archivo <- "resultados/vueltas_carrera_limpias.csv"
if (!file.exists(archivo)) {
  carreras <- sesiones[sesiones$nombre == "Race" & sesiones$presente_en_fuente %in% 1, ]
  todo <- list()
  for (m in carreras$meeting_key) {
    carrera <- leerCarreraGP(rutaBase, m)
    if (!isTRUE(carrera$verificada) || nrow(carrera$vueltas) == 0) next
    f <- aFechaUTC(carrera$clima$date)
    ll <- carrera$clima$rainfall[f >= aFechaUTC(carrera$sesion$fecha_inicio) & f <= aFechaUTC(carrera$sesion$fecha_fin)]
    if (length(ll) == 0 || !all(ll %in% 0) || any(carrera$stints$compound %in% c("INTERMEDIATE", "WET"))) next
    N <- ultimaVueltaReal(carrera$vueltas)
    for (d in unique(carrera$vueltas$driver_number)) {
      x <- vueltasCarreraPiloto(carrera, d)
      x <- x[x$limpia_carrera & x$compound %in% compuestosSecos & is.finite(x$edad_inicio) & x$lap_number <= N, ]
      if (nrow(x) < 10) next
      x <- x[x$lap_duration <= 1.07 * median(x$lap_duration), ]
      todo[[length(todo) + 1]] <- data.frame(meeting_key = m, temporada = grandesPremios$temporada[grandesPremios$meeting_key == m],
        evaluable = estadoCarreraEvaluable(carrera) == "ok", driver_number = d, lap_number = x$lap_number,
        compound = x$compound, stint = x$stint_number, edad = x$edad_inicio, t = x$lap_duration, N = N)
    }
  }
  write.csv(do.call(rbind, todo), archivo, row.names = FALSE)
}
v <- read.csv(archivo)
cat("Carreras:", length(unique(v$meeting_key)), " vueltas:", nrow(v), "\n")
cat("Edad maxima alcanzada por stint, cuantiles 50/90/99:\n")
mx <- aggregate(edad ~ meeting_key + driver_number + stint + compound, v, max)
print(t(sapply(split(mx$edad, mx$compound), quantile, c(.5, .9, .99))))

# Residualizar dentro de cada carrera; degradacion lineal por compuesto para "desgaste".
carreras <- split(v, v$meeting_key)
prep <- lapply(carreras, function(x) {
  x$compound <- factor(x$compound); x$piloto <- factor(x$driver_number)
  X <- if (nlevels(x$compound) > 1) model.matrix(~ piloto + compound + compound:edad + lap_number, x) else model.matrix(~ piloto + edad + lap_number, x)
  q <- qr(X)
  co <- qr.coef(q, x$t); tm <- median(x$t)
  deg <- sapply(compuestosSecos, function(k) { n <- if (nlevels(x$compound) > 1) paste0("compound", k, ":edad") else "edad"; if (n %in% names(co) && k %in% levels(x$compound)) 100 * co[[n]] / tm else NA })
  list(x = x, q = q, ry = qr.resid(q, x$t), deg = deg)
})
degTipica <- apply(sapply(prep, `[[`, "deg"), 1, function(z) median(z[is.finite(z) & z > 0]))
cat("Degradacion lineal tipica en carrera (% por vuelta):\n"); print(round(degTipica, 3))

estad <- function(forma, k, umbral, cuales = names(prep)) {
  sxy <- 0; sxx <- 0; syy <- 0; porCarrera <- list()
  for (n in cuales) {
    p <- prep[[n]]; x <- p$x
    es <- x$compound == k
    if (!any(es)) next
    d <- p$deg[[k]]; if (!is.finite(d) || d <= 0) d <- degTipica[[k]]
    z <- if (forma == "edad") x$edad else d * x$edad
    h <- ifelse(es, pmax(0, z - umbral)^2, 0)
    if (all(h == 0)) next
    rh <- qr.resid(p$q, h)
    porCarrera[[n]] <- c(sxy = sum(p$ry * rh), sxx = sum(rh^2))
    sxy <- sxy + sum(p$ry * rh); sxx <- sxx + sum(rh^2); syy <- syy + sum(p$ry^2)
  }
  list(c = if (sxx > 0) sxy / sxx else NA, mejora = if (sxx > 0) sxy^2 / sxx else 0, porCarrera = porCarrera)
}
grillas <- list(edad = 6:45, desgaste = seq(0.3, 3, by = 0.05))
res <- list()
for (forma in names(grillas)) for (k in compuestosSecos) {
  m <- sapply(grillas[[forma]], function(u) { e <- estad(forma, k, u); if (is.finite(e$c) && e$c > 0) e$mejora else 0 })
  u <- grillas[[forma]][which.max(m)]; e <- estad(forma, k, u)
  # Validacion dejando fuera cada carrera: umbral y c elegidos sin ella.
  ganancia <- 0; total <- 0
  for (n in names(prep)) {
    otras <- setdiff(names(prep), n)
    mo <- sapply(grillas[[forma]], function(uu) { ee <- estad(forma, k, uu, otras); if (is.finite(ee$c) && ee$c > 0) ee$mejora else 0 })
    uu <- grillas[[forma]][which.max(mo)]; cc <- estad(forma, k, uu, otras)$c
    pc <- estad(forma, k, uu, n)$porCarrera[[n]]
    if (!is.null(pc) && is.finite(cc)) ganancia <- ganancia + 2 * cc * pc[["sxy"]] - cc^2 * pc[["sxx"]]
  }
  res[[paste(forma, k)]] <- data.frame(forma = forma, compuesto = k, umbral = u, c = e$c, mejora_muestra = e$mejora, ganancia_validacion = ganancia)
}
r <- do.call(rbind, res); rownames(r) <- NULL
print(r, digits = 4)
write.csv(r, "resultados/caida_parametros.csv", row.names = FALSE)
