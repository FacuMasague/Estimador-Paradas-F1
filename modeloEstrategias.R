# Modelo de ritmo por compuesto, busqueda de estrategias y evaluacion contra la
# carrera real. Estas funciones NO leen SQLite ni muestran menus: reciben tablas y
# devuelven tablas, para poder probarlas con datos sinteticos.
# Unidades: tiempos en segundos; edades, vueltas y paradas en vueltas enteras.
# "Mejor estrategia" significa menor tiempo proyectado DENTRO de este modelo y sus
# supuestos, no una garantia de optimalidad en una carrera real.

compuestosSecos <- c("SOFT", "MEDIUM", "HARD")
coloresCompuestos <- c(SOFT = "#d62728", MEDIUM = "#e0a800", HARD = "#7f7f7f")

# Pendiente de Theil-Sen DENTRO de cada stint: mediana de las pendientes entre pares
# de vueltas del mismo stint. No compara vueltas de stints distintos, porque cada
# stint puede tener otra carga de combustible, otro piloto u otra pista.
pendienteIntraStint <- function(edad, tiempo, stint) {
  pendientes <- unlist(lapply(split(seq_along(edad), stint), function(f) {
    if (length(f) < 2) return(NULL)
    pares <- combn(f, 2)
    i <- pares[1, ]
    j <- pares[2, ]
    distintas <- edad[i] != edad[j]
    (tiempo[j] - tiempo[i])[distintas] / (edad[j] - edad[i])[distintas]
  }))
  if (length(pendientes) == 0) return(NA_real_)
  median(pendientes)
}

# Modelo t_c(a) = base_c + pendiente_c * a para SOFT, MEDIUM y HARD, en tres pasos:
# 1) degradacionCampo(): degradacion PORCENTUAL de cada compuesto con TODOS los
#    pilotos del fin de semana (pendiente / base de cada piloto, mediana entre pilotos).
# 2) basesCampo(): base de cada piloto y compuesto, solo con sus stints.
# 3) modeloPiloto(): base propia del piloto elegido (o la cadena de respaldo) y
#    pendiente = base * degradacion del campo. modeloPilotoCalibrado() es la version
#    vigente: corrige practica -> carrera con constantes medidas en otras carreras.
# Las vueltas llegan de revisarPracticasPiloto() con apta_ajuste, edad_inicio y
# tiempo_corregido (combustible estimado restado: tiempo con tanque casi vacio).
aptasCompuestos <- function(v) {
  columnas <- c("session_key", "driver_number", "stint_number", "compound",
                "edad_inicio", "tiempo_corregido")
  if (is.null(v) || nrow(v) == 0) {
    return(data.frame(session_key = integer(), driver_number = integer(),
                      stint_number = integer(), compound = character(),
                      edad_inicio = numeric(), tiempo_corregido = numeric()))
  }
  v[v$apta_ajuste %in% TRUE & v$compound %in% compuestosSecos &
      is.finite(v$tiempo_corregido) & is.finite(v$edad_inicio), columnas]
}

# Intercepto a edad 0: mediana por stint de (tiempo - pendiente * edad) y mediana
# entre stints. Cada stint de practica pudo tener otro programa de trabajo.
interceptoStints <- function(p, pendiente) {
  median(tapply(p$tiempo_corregido - pendiente * p$edad_inicio,
                paste(p$session_key, p$stint_number), median))
}

# Degradacion porcentual por compuesto con todos los pilotos del GP.
# Un piloto aporta si tiene al menos minAptas vueltas aptas con minRango de edades;
# su degradacion es pendiente intra-stint / base propia. Se usa la mediana entre
# pilotos y se exige al menos minPilotos. Supuesto declarado: todos los autos
# degradan un porcentaje parecido; las diferencias de auto y de estilo se pierden.
# minAptas y minRango (8 y 5 por defecto) son umbrales configurables, no validados.
degradacionCampo <- function(aptas, minAptas = 8, minRango = 5, minPilotos = 3) {
  filas <- lapply(compuestosSecos, function(cpd) {
    x <- aptas[aptas$compound == cpd, ]
    porcentajes <- numeric()
    edades <- numeric()
    for (d in unique(x$driver_number)) {
      p <- x[x$driver_number == d, ]
      if (nrow(p) < minAptas || diff(range(p$edad_inicio)) < minRango) next
      pendiente <- pendienteIntraStint(p$edad_inicio, p$tiempo_corregido,
                                       paste(p$session_key, p$stint_number))
      if (!is.finite(pendiente)) next
      porcentajes <- c(porcentajes, 100 * pendiente / interceptoStints(p, pendiente))
      edades <- c(edades, p$edad_inicio)
    }
    suficiente <- length(porcentajes) >= minPilotos
    data.frame(compound = cpd, pilotos = length(porcentajes),
               degradacion_pct = if (suficiente) median(porcentajes) else NA_real_,
               edad_max_observada = if (suficiente) max(edades) else NA_real_)
  })
  do.call(rbind, filas)
}

# Base de cada piloto y compuesto con al menos minAptasBase vueltas aptas propias.
# Para quitar la degradacion se usa la del campo: primero con la mediana de los
# tiempos del piloto y despues con la base asi obtenida.
basesCampo <- function(aptas, degradacion, minAptasBase = 3) {
  filas <- list()
  for (cpd in compuestosSecos) {
    pct <- degradacion$degradacion_pct[degradacion$compound == cpd]
    if (!is.finite(pct)) next
    x <- aptas[aptas$compound == cpd, ]
    for (d in unique(x$driver_number)) {
      p <- x[x$driver_number == d, ]
      if (nrow(p) < minAptasBase) next
      base <- median(p$tiempo_corregido)
      for (iteracion in 1:2) base <- interceptoStints(p, base * pct / 100)
      filas[[length(filas) + 1]] <- data.frame(driver_number = d, compound = cpd,
                                               base = base, vueltas = nrow(p))
    }
  }
  if (length(filas) == 0) {
    return(data.frame(driver_number = integer(), compound = character(),
                      base = numeric(), vueltas = integer()))
  }
  do.call(rbind, filas)
}

# Modelo del piloto elegido. Base de cada compuesto, en este orden:
# 1. "propia": la del piloto.
# 2. "campo_pareado": base propia del MEDIUM x mediana de (base del compuesto / base
#    del MEDIUM) de los pilotos que tienen ambas en ese GP (al menos minPilotos).
#    Comparar dentro de cada piloto evita mezclar autos y pilotos distintos.
# 3. "companero": igual, con el o los companeros de equipo que tengan ambas.
# 4. "igual_medio" (solo HARD): base del HARD = base del MEDIUM. Escenario
#    condicionado: con FastF1 (2019-2026) no se encontro una diferencia estable.
# El MEDIUM es la referencia: sin base propia de MEDIUM solo hay bases propias.
# Pendiente: base * degradacion del campo. Si el campo no alcanza para el HARD se usa
# la degradacion porcentual del MEDIUM ("campo_medio"), con el mismo supuesto.
# pendienteMinima: la pendiente que se USA en las estrategias no baja de este valor.
modeloPiloto <- function(dorsal, bases, degradacion, companeros = integer(),
                         minPilotos = 3, pendienteMinima = 0) {
  baseDe <- function(d, cpd) {
    b <- bases$base[bases$driver_number %in% d & bases$compound == cpd]
    if (length(b) == 1) b else NA_real_
  }
  cocientes <- function(pilotos, cpd) {
    r <- vapply(pilotos, function(d) baseDe(d, cpd) / baseDe(d, "MEDIUM"), numeric(1))
    r[is.finite(r)]
  }
  baseMedio <- baseDe(dorsal, "MEDIUM")
  otros <- setdiff(unique(bases$driver_number), dorsal)
  filas <- lapply(compuestosSecos, function(cpd) {
    base <- baseDe(dorsal, cpd)
    origenBase <- if (is.finite(base)) "propia" else NA_character_
    pilotosBase <- 0L
    if (!is.finite(base) && cpd != "MEDIUM" && is.finite(baseMedio)) {
      campo <- cocientes(otros, cpd)
      equipo <- cocientes(intersect(companeros, otros), cpd)
      if (length(campo) >= minPilotos) {
        base <- baseMedio * median(campo)
        origenBase <- "campo_pareado"
        pilotosBase <- length(campo)
      } else if (length(equipo) > 0) {
        base <- baseMedio * median(equipo)
        origenBase <- "companero"
        pilotosBase <- length(equipo)
      } else if (cpd == "HARD") {
        base <- baseMedio
        origenBase <- "igual_medio"
      }
    }
    deg <- degradacion[degradacion$compound == cpd, ]
    origenPendiente <- "campo"
    if (!is.finite(deg$degradacion_pct) && cpd == "HARD") {
      deg <- degradacion[degradacion$compound == "MEDIUM", ]
      origenPendiente <- "campo_medio"
    }
    if (!is.finite(deg$degradacion_pct)) origenPendiente <- NA_character_
    pendiente <- base * deg$degradacion_pct / 100
    data.frame(
      compound = cpd,
      suficiente = is.finite(base) && is.finite(pendiente),
      origen_base = origenBase,
      origen_pendiente = origenPendiente,
      base = base,
      degradacion_pct = deg$degradacion_pct,
      pendiente = pendiente,
      pendiente_usada = if (is.finite(pendiente)) max(pendiente, pendienteMinima) else NA_real_,
      vueltas_propias = sum(bases$vueltas[bases$driver_number %in% dorsal &
                                            bases$compound == cpd]),
      pilotos_pendiente = deg$pilotos,
      pilotos_base = pilotosBase,
      edad_max_observada = deg$edad_max_observada
    )
  })
  do.call(rbind, filas)
}

# Modelo CALIBRADO del piloto elegido (version vigente). Las practicas exageran la
# degradacion y su diferencia de base entre compuestos no se relaciona con la de
# carrera, asi que se aplican constantes medidas una vez con carreras que NO son
# casos de investigacion (ver calibracion en el README):
# - Base: MEDIUM propio; SOFT y HARD = MEDIUM x (1 + difBase / 100).
# - Degradacion %: la del campo x factorDegradacion del compuesto. Si el campo no
#   alcanza para SOFT o HARD: la del MEDIUM del campo x factor del MEDIUM x
#   relacionDegradacion (cociente de degradacion en carrera respecto del MEDIUM).
# - Nivel: la base del MEDIUM se multiplica por (1 + nivelBase / 100), porque el ritmo
#   de carrera es mas rapido que el de practicas. Es un ajuste promedio medido con GP
#   que no son casos; su causa (motor, carga, programa de practicas) no se pudo aislar.
# calibracion: lista con factorDegradacion (SOFT, MEDIUM, HARD), difBase (SOFT, HARD),
# relacionDegradacion (SOFT, HARD) y nivelBase (%; opcional, 0 si no esta).
modeloPilotoCalibrado <- function(dorsal, bases, degradacion, calibracion,
                                  pendienteMinima = 0) {
  baseMedio <- bases$base[bases$driver_number %in% dorsal & bases$compound == "MEDIUM"]
  baseMedio <- if (length(baseMedio) == 1) baseMedio else NA_real_
  nivel <- if (is.null(calibracion$nivelBase)) 0 else calibracion$nivelBase
  basePractica <- baseMedio
  baseMedio <- baseMedio * (1 + nivel / 100)
  degMedio <- degradacion[degradacion$compound == "MEDIUM", ]
  filas <- lapply(compuestosSecos, function(cpd) {
    if (cpd == "MEDIUM") {
      base <- baseMedio
      origenBase <- "propia"
    } else {
      base <- baseMedio * (1 + calibracion$difBase[[cpd]] / 100)
      origenBase <- "medio_calibrado"
    }
    deg <- degradacion[degradacion$compound == cpd, ]
    if (is.finite(deg$degradacion_pct)) {
      pct <- deg$degradacion_pct * calibracion$factorDegradacion[[cpd]]
      pctPractica <- deg$degradacion_pct
      origenPendiente <- "campo_calibrado"
    } else if (cpd != "MEDIUM" && is.finite(degMedio$degradacion_pct)) {
      pct <- degMedio$degradacion_pct * calibracion$factorDegradacion[["MEDIUM"]] *
        calibracion$relacionDegradacion[[cpd]]
      pctPractica <- degMedio$degradacion_pct
      origenPendiente <- "medio_calibrado"
      deg <- degMedio
    } else {
      pct <- NA_real_
      pctPractica <- NA_real_
      origenPendiente <- NA_character_
    }
    pendiente <- base * pct / 100
    data.frame(
      compound = cpd,
      suficiente = is.finite(base) && is.finite(pendiente),
      origen_base = if (is.finite(base)) origenBase else NA_character_,
      origen_pendiente = origenPendiente,
      base = base,
      base_practica = basePractica,
      degradacion_pct = pct,
      degradacion_practica_pct = pctPractica,
      pendiente = pendiente,
      pendiente_usada = if (is.finite(pendiente)) max(pendiente, pendienteMinima) else NA_real_,
      vueltas_propias = sum(bases$vueltas[bases$driver_number %in% dorsal &
                                            bases$compound == cpd]),
      pilotos_pendiente = deg$pilotos,
      pilotos_base = 0L,
      edad_max_observada = deg$edad_max_observada
    )
  })
  do.call(rbind, filas)
}

# Caida final del neumatico: pasada la edad caida_edad, cada vuelta suma
# caida_coef * (edad - caida_edad)^2 segundos. Sin estas columnas no hay caida.
# caida: lista con forma ("edad" o "desgaste"), umbral y coef por compuesto, medidos en
# carrera (resultados/caida.R). Con "desgaste" la caida empieza cuando la degradacion
# acumulada (degradacion % x edad) llega al umbral, y el coeficiente se pasa a edad.
agregarCaida <- function(modelo, caida) {
  modelo$caida_edad <- Inf
  modelo$caida_coef <- 0
  if (is.null(caida)) return(modelo)
  for (j in seq_len(nrow(modelo))) {
    k <- modelo$compound[j]
    if (caida$forma == "edad") {
      modelo$caida_edad[j] <- caida$umbral[[k]]
      modelo$caida_coef[j] <- caida$coef[[k]]
    } else {
      d <- modelo$degradacion_pct[j]
      if (is.finite(d) && d > 0) {
        modelo$caida_edad[j] <- caida$umbral[[k]] / d
        modelo$caida_coef[j] <- caida$coef[[k]] * d^2
      }
    }
  }
  modelo
}

# Tiempo de un stint de n vueltas con un juego de edad inicial a0:
# suma_{k=0}^{n-1} (base + pendiente * (a0 + k) + caida). La edad es al INICIO de
# cada vuelta. n puede ser un vector (un stint por elemento).
tiempoStint <- function(base, pendiente, edadInicial, n, caidaEdad = Inf, caidaCoef = 0) {
  total <- n * base + pendiente * (n * edadInicial + n * (n - 1) / 2)
  if (caidaCoef > 0 && is.finite(caidaEdad) && length(n) > 0 && max(n) > 0) {
    for (k in seq_len(max(n)) - 1) {
      total <- total + (k < n) * caidaCoef * pmax(0, edadInicial + k - caidaEdad)^2
    }
  }
  total
}

# Caida de un compuesto del modelo (sin columnas, no hay caida).
caidaDe <- function(m) {
  if (is.null(m$caida_edad)) c(Inf, 0) else c(m$caida_edad, m$caida_coef)
}

# Generar y evaluar todas las estrategias admisibles.
# Convencion: parar al final de la vuelta L significa completar L vueltas con un juego
# y empezar la L+1 con el siguiente.
# Restricciones explicitas: paradas tras vueltas enteras, stints de al menos
# minVueltasStint vueltas, solo compuestos con modelo suficiente y al menos dos
# compuestos secos distintos (reglamento deportivo 2025 art. 30 y 2026 B6; 2023-2024
# no verificado aqui). edadInicial: edad de cada juego al montarlo (0 = nuevo).
# El combustible quemado en carrera depende de la vuelta de carrera y suma lo mismo a
# todas las estrategias: no cambia la comparacion y no se incluye en tiempo_modelo.
# tiempo_modelo es relativo: sirve para comparar estrategias, no como tiempo real.
# maxFactorEdad: un stint no puede terminar con una edad mayor a maxFactorEdad veces
# la edad maxima observada en practicas para ese compuesto. Es un limite de
# extrapolacion configurable, no una vida maxima demostrada del neumatico.
# Dentro de ese limite, superar la edad observada se informa en extrapola.
# vidaMaxima: vueltas maximas de un juego por compuesto, contando su edad al montarlo
# (edad inicial + largo del stint). Es lo que los equipos casi nunca superan en carrera
# (percentil 95), no una vida fisica medida: la caida final no se observa en los datos
# porque los equipos paran antes. NULL = sin limite.
generarEstrategias <- function(nVueltas, modelo, perdidaBoxes, paradas = 1:2,
                               minVueltasStint = 5,
                               edadInicial = c(SOFT = 0, MEDIUM = 0, HARD = 0),
                               maxFactorEdad = 3, vidaMaxima = NULL) {
  disponibles <- modelo$compound[modelo$suficiente]
  resultados <- list()
  for (p in paradas) {
    secuencias <- expand.grid(rep(list(disponibles), p + 1), stringsAsFactors = FALSE)
    secuencias <- secuencias[apply(secuencias, 1, function(s) length(unique(s)) >= 2), ,
                             drop = FALSE]
    if (nrow(secuencias) == 0) next

    # Vueltas de parada posibles: cada stint con al menos minVueltasStint vueltas.
    rango <- seq.int(minVueltasStint, nVueltas - minVueltasStint)
    if (p == 1) {
      cortes <- matrix(rango, ncol = 1)
    } else {
      cortes <- as.matrix(expand.grid(rango, rango))
      cortes <- cortes[cortes[, 2] - cortes[, 1] >= minVueltasStint &
                         nVueltas - cortes[, 2] >= minVueltasStint, , drop = FALSE]
    }
    if (nrow(cortes) == 0) next
    largos <- cbind(cortes[, 1], if (p == 2) cortes[, 2] - cortes[, 1],
                    nVueltas - cortes[, p])

    for (s in seq_len(nrow(secuencias))) {
      cpd <- unlist(secuencias[s, ])
      total <- p * perdidaBoxes
      excede <- rep(FALSE, nrow(largos))
      admisible <- rep(TRUE, nrow(largos))
      for (j in seq_along(cpd)) {
        m <- modelo[modelo$compound == cpd[j], ]
        a0 <- edadInicial[[cpd[j]]]
        cd <- caidaDe(m)
        total <- total + tiempoStint(m$base, m$pendiente_usada, a0, largos[, j], cd[1], cd[2])
        edadFinal <- a0 + largos[, j] - 1
        # Extrapolacion: la ultima vuelta del stint supera la edad observada.
        excede <- excede | (edadFinal > m$edad_max_observada)
        admisible <- admisible & (edadFinal <= maxFactorEdad * m$edad_max_observada)
        if (!is.null(vidaMaxima)) admisible <- admisible & (a0 + largos[, j] <= vidaMaxima[[cpd[j]]])
      }
      if (!any(admisible)) next
      resultados[[length(resultados) + 1]] <- data.frame(
        paradas = p,
        secuencia = paste(cpd, collapse = "-"),
        vueltas_parada = apply(cortes[admisible, , drop = FALSE], 1, paste, collapse = "-"),
        stints = apply(largos[admisible, , drop = FALSE], 1, paste, collapse = "+"),
        tiempo_modelo = total[admisible],
        extrapola = excede[admisible]
      )
    }
  }
  if (length(resultados) == 0) {
    return(data.frame(paradas = integer(), secuencia = character(),
                      vueltas_parada = character(), stints = character(),
                      tiempo_modelo = numeric(), extrapola = logical()))
  }
  estrategias <- do.call(rbind, resultados)
  estrategias[order(estrategias$tiempo_modelo), ]
}

# Margen de empate por la incertidumbre de la calibracion (Monte Carlo).
# Las constantes de calibracion tienen error: en cada simulacion se suma a la
# degradacion % de cada compuesto y a la diferencia de base de SOFT y HARD un residuo
# elegido al azar entre los residuos medidos al calibrar (dejando fuera cada GP), y
# se vuelven a buscar todas las estrategias. Los residuos de cada compuesto se
# sortean por separado (supuesto: errores independientes); la base del MEDIUM y la
# perdida en boxes se dejan fijas.
# Devuelve:
# - margen: cuantil (0,9) de lo que pierde la estrategia recomendada frente a la mejor
#   de cada simulacion. Diferencias del modelo menores al margen son empates.
# - prob_real_mejor: proporcion de simulaciones en las que la estrategia real (si se
#   pasa en real) es mas rapida que la recomendada.
# recomendada / real: list(compuestos, largos).
margenEmpate <- function(modelo, nVueltas, perdidaBoxes, residuos, recomendada, real = NULL,
                         paradas = 1:2, minVueltasStint = 5,
                         edadInicial = c(SOFT = 0, MEDIUM = 0, HARD = 0), maxFactorEdad = 3,
                         simulaciones = 200, cuantil = 0.9, semilla = 1, vidaMaxima = NULL) {
  set.seed(semilla)
  baseMedio <- modelo$base[modelo$compound == "MEDIUM"]
  perdidas <- numeric(simulaciones)
  realMejor <- logical(simulaciones)
  for (b in seq_len(simulaciones)) {
    m <- modelo
    for (j in seq_len(nrow(m))) {
      cpd <- m$compound[j]
      if (cpd %in% names(residuos$difBase)) {
        m$base[j] <- m$base[j] + baseMedio * sample(residuos$difBase[[cpd]], 1) / 100
      }
      pct <- m$degradacion_pct[j] + sample(residuos$degradacion[[cpd]], 1)
      m$pendiente[j] <- m$base[j] * pct / 100
      m$pendiente_usada[j] <- max(m$pendiente[j], 0)
    }
    est <- generarEstrategias(nVueltas, m, perdidaBoxes, paradas, minVueltasStint,
                              edadInicial, maxFactorEdad, vidaMaxima)
    tRec <- tiempoEstrategia(recomendada$compuestos, recomendada$largos, m, perdidaBoxes,
                             edadInicial)
    perdidas[b] <- tRec - min(est$tiempo_modelo)
    if (!is.null(real)) {
      realMejor[b] <- tiempoEstrategia(real$compuestos, real$largos, m, perdidaBoxes,
                                       edadInicial) < tRec
    }
  }
  list(margen = unname(quantile(perdidas, cuantil)), perdidas = perdidas,
       prob_real_mejor = if (is.null(real)) NA_real_ else mean(realMejor))
}

# Combinaciones (paradas y compuestos usados) cuya mejor estrategia queda a menos de
# margen segundos de la mejor del modelo: el modelo no puede distinguirlas.
combinacionesEmpatadas <- function(estrategias, margen) {
  combinacion <- sapply(strsplit(estrategias$secuencia, "-"),
                        function(x) paste(sort(unique(x)), collapse = "+"))
  clave <- paste(estrategias$paradas, "parada(s):", combinacion)
  mejor <- tapply(estrategias$tiempo_modelo, clave, min)
  dentro <- mejor[mejor <= min(estrategias$tiempo_modelo) + margen]
  names(sort(dentro))
}

# Elegir hasta k estrategias DISTINTAS: la mejor de cada combinacion de cantidad de
# paradas y compuestos usados. Variantes que solo mueven la parada una vuelta no
# cuentan como alternativas distintas. Tampoco las que usan los mismos compuestos en
# otro orden: en este modelo (juegos nuevos, combustible igual para todas) el orden de
# los stints no cambia el tiempo total.
mejoresEstrategias <- function(estrategias, k = 3) {
  if (nrow(estrategias) == 0) return(estrategias)
  estrategias <- estrategias[order(estrategias$tiempo_modelo), ]
  combinacion <- sapply(strsplit(estrategias$secuencia, "-"),
                        function(x) paste(sort(x), collapse = "-"))
  unicas <- estrategias[!duplicated(paste(estrategias$paradas, combinacion)), ]
  mejores <- head(unicas, k)
  mejores$diferencia <- mejores$tiempo_modelo - mejores$tiempo_modelo[1]
  rownames(mejores) <- NULL
  mejores
}

# Tiempo del modelo para una estrategia dada por sus stints (compound, vueltas).
tiempoEstrategia <- function(compuestos, largos, modelo, perdidaBoxes,
                             edadInicial = c(SOFT = 0, MEDIUM = 0, HARD = 0)) {
  total <- (length(largos) - 1) * perdidaBoxes
  for (j in seq_along(compuestos)) {
    m <- modelo[modelo$compound == compuestos[j], ]
    if (nrow(m) != 1 || !m$suficiente) return(NA_real_)
    cd <- caidaDe(m)
    total <- total + tiempoStint(m$base, m$pendiente_usada, edadInicial[[compuestos[j]]],
                                 largos[j], cd[1], cd[2])
  }
  total
}

# Perdida por parada bajo bandera verde: (vuelta de entrada + vuelta de salida) menos
# dos vueltas de referencia del mismo piloto. No es solo el tiempo detenido ni toda la
# permanencia en pit lane: es el tiempo adicional frente a seguir en pista.
# vueltas: vueltas de carrera de TODOS los pilotos de una edicion, con
#   driver_number, lap_number, lap_duration, is_pit_out_lap, cambio_neumatico
#   (la vuelta de salida empieza un stint nuevo) y neutralizada (bandera roja o VSC/SC).
# Referencia: mediana de hasta 5 vueltas antes de la entrada y 5 despues de la salida,
# sin boxes, sin la vuelta 1 y sin neutralizacion; se exigen al menos 3.
calcularPerdidasParadas <- function(vueltas, ventana = 5, minReferencias = 3) {
  perdidas <- numeric()
  for (piloto in unique(vueltas$driver_number)) {
    v <- vueltas[vueltas$driver_number %in% piloto, ]
    v <- v[order(v$lap_number), ]
    salidas <- v$lap_number[v$is_pit_out_lap %in% 1 & v$cambio_neumatico %in% TRUE &
                              v$lap_number > 2]
    noUsables <- c(v$lap_number[v$is_pit_out_lap %in% 1],
                   v$lap_number[v$is_pit_out_lap %in% 1] - 1, 1)
    for (s in salidas) {
      entrada <- v[v$lap_number == s - 1, ]
      salida <- v[v$lap_number == s, ]
      if (nrow(entrada) != 1 || nrow(salida) != 1) next
      if (entrada$neutralizada %in% TRUE || salida$neutralizada %in% TRUE) next
      if (!all(is.finite(c(entrada$lap_duration, salida$lap_duration)))) next
      cerca <- v$lap_number >= s - 1 - ventana & v$lap_number <= s + ventana &
        !v$lap_number %in% noUsables & !(v$neutralizada %in% TRUE) &
        is.finite(v$lap_duration)
      if (sum(cerca) < minReferencias) next
      referencia <- median(v$lap_duration[cerca])
      perdidas <- c(perdidas, entrada$lap_duration + salida$lap_duration - 2 * referencia)
    }
  }
  perdidas
}

# Tiempos de vuelta previstos para una estrategia concreta, vuelta por vuelta:
# t_i = base_c + pendiente_c * edad_i + sensibilidad * carga_i.
# La base es el tiempo con tanque casi vacio; carga_i es el combustible al empezar la
# vuelta i: consumo * (nVueltas - i + 1 + margenVueltas), en kg.
# stints: data.frame con compound, lap_start, lap_end y tyre_age_at_start.
# combustible: lista con sensibilidad (s/kg), consumo (kg/vuelta) y margenVueltas.
predecirVueltas <- function(stints, modelo, combustible, nVueltas) {
  filas <- lapply(seq_len(nrow(stints)), function(j) {
    vueltas <- seq.int(stints$lap_start[j], stints$lap_end[j])
    m <- modelo[modelo$compound == stints$compound[j], ]
    edad <- stints$tyre_age_at_start[j] + (vueltas - stints$lap_start[j])
    carga <- combustible$consumo * (nVueltas - vueltas + 1 + combustible$margenVueltas)
    prediccion <- if (nrow(m) == 1 && m$suficiente) {
      cd <- caidaDe(m)
      m$base + m$pendiente_usada * edad + cd[2] * pmax(0, edad - cd[1])^2 +
        combustible$sensibilidad * carga
    } else {
      rep(NA_real_, length(vueltas))
    }
    data.frame(lap_number = vueltas, compound = stints$compound[j], edad = edad,
               prediccion = prediccion)
  })
  do.call(rbind, filas)
}

# Comparar los tiempos previstos con los observados en la carrera real.
# observadas: vueltas limpias del piloto en carrera (lap_number, lap_duration).
# Dos medidas:
# - mae_absoluto: error absoluto medio tal cual. Incluye el desplazamiento de nivel
#   (la carga de practica es solo estimada; tambien cambian la pista, el modo de motor
#   y el trafico).
# - mae_ajustado: tras restar UN unico desplazamiento para toda la carrera (la mediana
#   de los errores). Evalua la degradacion y las diferencias entre compuestos.
compararVueltas <- function(prediccion, observadas) {
  x <- merge(prediccion, observadas[, c("lap_number", "lap_duration")], by = "lap_number")
  x <- x[is.finite(x$prediccion) & is.finite(x$lap_duration), ]
  x <- x[order(x$lap_number), ]
  if (nrow(x) == 0) {
    return(list(vueltas = x, n = 0, mae_absoluto = NA_real_,
                desplazamiento = NA_real_, mae_ajustado = NA_real_))
  }
  error <- x$lap_duration - x$prediccion
  desplazamiento <- median(error)
  x$error <- error
  x$error_ajustado <- error - desplazamiento
  list(vueltas = x, n = nrow(x), mae_absoluto = mean(abs(error)),
       desplazamiento = desplazamiento, mae_ajustado = mean(abs(error - desplazamiento)))
}

# Puesto de una estrategia (por su tiempo del modelo) entre todas las generadas.
puestoEstrategia <- function(tiempo, estrategias) {
  if (!is.finite(tiempo)) return(NA_integer_)
  1L + sum(estrategias$tiempo_modelo < tiempo)
}

# Graficos de la version de investigacion, con graficos base de R.
# 1) Tiempos de vuelta de la carrera real frente a la prediccion para la estrategia real.
# 2) Diferencia de tiempo del modelo: mejores estrategias y estrategia real.
# 3) Vueltas aptas propias de practica (combustible corregido) y rectas del modelo.
graficarCaso <- function(caso) {
  op <- par(mfrow = if (is.null(caso$sc)) c(1, 3) else c(2, 2), mar = c(4.5, 4.5, 3, 1))
  on.exit(par(op))

  # 1. Carrera real contra prediccion.
  comp <- caso$comparacion$vueltas
  todas <- caso$vueltasCarrera
  rangoY <- range(c(comp$lap_duration, comp$prediccion + caso$comparacion$desplazamiento),
                  na.rm = TRUE)
  plot(todas$lap_number, todas$lap_duration, col = "grey80", pch = 20,
       ylim = rangoY + c(-0.5, 0.5), xlab = "Vuelta de carrera", ylab = "Tiempo (s)",
       main = "Carrera real vs. modelo")
  points(comp$lap_number, comp$lap_duration, pch = 19,
         col = coloresCompuestos[comp$compound])
  # Una linea por stint, para no unir el final de un stint con el comienzo del siguiente.
  tramo <- cumsum(c(TRUE, diff(comp$lap_number) > 1 | comp$compound[-1] != comp$compound[-nrow(comp)] |
                      diff(comp$edad) < 0))
  for (t in unique(tramo)) {
    x <- comp[tramo == t, ]
    lines(x$lap_number, x$prediccion + caso$comparacion$desplazamiento, lwd = 2)
    # Recta real del stint: Theil-Sen de los tiempos observados contra la vuelta.
    if (nrow(x) >= 3) {
      pendiente <- pendienteIntraStint(x$lap_number, x$lap_duration, rep(1, nrow(x)))
      corte <- median(x$lap_duration - pendiente * x$lap_number)
      lines(range(x$lap_number), corte + pendiente * range(x$lap_number), lwd = 2, lty = 2,
            col = coloresCompuestos[x$compound[1]])
    }
  }
  abline(v = caso$paradasReales + 0.5, lty = 1, col = "black")
  abline(v = caso$paradasRecomendadas + 0.5, lty = 3, col = "blue", lwd = 2)
  legend("topright", bty = "n", cex = 0.8,
         legend = c("Observada (limpia)", "Prediccion + desplazamiento", "Recta real del stint",
                    "Parada real", "Parada recomendada"),
         pch = c(19, NA, NA, NA, NA), lty = c(NA, 1, 2, 1, 3), lwd = c(NA, 2, 2, 1, 2),
         col = c("black", "black", "grey40", "black", "blue"))

  # 2. Estrategias en el modelo.
  barras <- c(setNames(caso$mejores$diferencia,
                       paste0(caso$mejores$secuencia, " (", caso$mejores$vueltas_parada, ")")),
              setNames(caso$diferenciaReal,
                       paste0("REAL ", caso$secuenciaReal, " (", caso$vueltasParadaReal, ")")))
  par(mar = c(4.5, 13, 3, 1))
  posiciones <- barplot(rev(barras), horiz = TRUE, las = 1, cex.names = 0.75,
                        col = rev(c(rep("steelblue", length(barras) - 1), "darkorange")),
                        xlim = c(0, max(barras, caso$margen, 1, na.rm = TRUE) * 1.15),
                        xlab = "Diferencia con la mejor del modelo (s)",
                        main = "Estrategias (modelo)")
  text(rev(barras), posiciones, sprintf("%.1f s", rev(barras)), pos = 4, cex = 0.8)
  if (is.finite(caso$margen)) {
    abline(v = caso$margen, lty = 2, col = "grey30")
    mtext(sprintf("margen de empate %.1f s", caso$margen), side = 3, line = 0.2, cex = 0.7)
  }
  par(mar = c(4.5, 4.5, 3, 1))

  # 3. Degradacion estimada con las practicas.
  pr <- caso$practicaAptas
  m0 <- caso$modelo[caso$modelo$suficiente, ]
  plot(pr$edad_inicio, pr$tiempo_corregido, type = "n",
       xlim = c(0, max(pr$edad_inicio)),
       ylim = range(c(pr$tiempo_corregido, m0$base, m0$base_practica), na.rm = TRUE),
       xlab = "Edad del neumatico (vueltas)", ylab = "Tiempo corregido (s)",
       main = "Practicas: datos y rectas")
  for (cpd in compuestosSecos) {
    x <- pr[pr$compound == cpd, ]
    points(x$edad_inicio, x$tiempo_corregido, col = coloresCompuestos[cpd], pch = 19)
    m <- caso$modelo[caso$modelo$compound == cpd, ]
    # Linea continua: la tendencia de practica con la que se saco la base del MEDIUM.
    # Punteada: la recta que usa la carrera (degradacion calibrada).
    if (m$suficiente) {
      abline(a = m$base, b = m$pendiente, col = coloresCompuestos[cpd], lwd = 2, lty = 2)
      if (cpd == "MEDIUM" && !is.null(m$degradacion_practica_pct)) {
        abline(a = m$base_practica, b = m$base_practica * m$degradacion_practica_pct / 100,
               col = coloresCompuestos[cpd], lwd = 2)
      }
    }
  }
  legend("topleft", bty = "n", cex = 0.8, legend = c(compuestosSecos, "tendencia en practica", "recta usada en carrera"),
         col = c(coloresCompuestos, "black", "black"), pch = c(15, 15, 15, NA, NA), lty = c(NA, NA, NA, 1, 2))

  # 4. Safety car: ahorro de parar si un SC empieza en cada vuelta (estrategia recomendada).
  if (!is.null(caso$sc)) {
    d <- caso$sc[caso$sc$tipo == "SC", ]
    a <- pmax(pmin(d$ahorro, 40), -40)
    plot(d$vuelta, a, type = "n", xlab = "Vuelta en que empieza el SC", ylab = "Ahorro de parar (s)",
         main = "Safety car: parar o seguir", ylim = range(c(a, 0), na.rm = TRUE) + c(-1, 1))
    parar <- d$accion == "parar"
    rect(d$vuelta[parar] - 0.5, par("usr")[3], d$vuelta[parar] + 0.5, par("usr")[4],
         col = "#2e7d3233", border = NA)
    lines(d$vuelta, a, lwd = 2)
    abline(h = 0, lty = 2)
    abline(v = caso$paradasRecomendadas + 0.5, lty = 3, col = "blue", lwd = 2)
    legend("topright", bty = "n", cex = 0.8, fill = c("#2e7d3233", NA), border = c(NA, NA),
           lty = c(NA, 3), col = c(NA, "blue"), lwd = c(NA, 2),
           legend = c("Conviene parar", "Parada recomendada"))
  }
}

# ---------------------------------------------------------------------------
# Neutralizaciones (VSC y SC). Funciones sin SQLite: reciben tablas.
# ---------------------------------------------------------------------------

# Reconstruir episodios de VSC y SC desde los mensajes de direccion de carrera de una
# carrera (control: date, lap_number, flag, message). Se cuentan episodios, no mensajes.
# - VSC: "VIRTUAL SAFETY CAR DEPLOYED" o "VSC DEPLOYED" hasta "... ENDING".
# - SC: "SAFETY CAR DEPLOYED" hasta "SAFETY CAR IN THIS LAP" (el SC entra al final de
#   esa vuelta, que todavia es neutralizada).
# - Un VSC seguido de un SC sin ENDING intermedio es una conversion: el VSC termina
#   cuando empieza el SC y queda marcado como convertido.
# - Una bandera roja o la bandera a cuadros cierran el episodio abierto.
# vueltas: vuelta de inicio y fin (vuelta del lider, segun la fuente) y cantidad de
# vueltas afectadas. cerrado = FALSE si no aparecio el mensaje de fin.
episodiosNeutralizacion <- function(control) {
  vacio <- data.frame(tipo = character(), vuelta_inicio = numeric(), vuelta_fin = numeric(),
                      vueltas = numeric(), desde = as.POSIXct(character(), tz = "UTC"),
                      hasta = as.POSIXct(character(), tz = "UTC"), segundos = numeric(),
                      cierre = character(), convertido = logical())
  if (is.null(control) || nrow(control) == 0) return(vacio)
  fecha <- as.POSIXct(control$date, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  control <- control[order(fecha), ]
  fecha <- sort(fecha)
  m <- toupper(ifelse(is.na(control$message), "", control$message))
  bandera <- ifelse(is.na(control$flag), "", control$flag)
  inicioVSC <- grepl("VIRTUAL SAFETY CAR DEPLOYED|^VSC DEPLOYED", m)
  finVSC <- grepl("VIRTUAL SAFETY CAR ENDING|^VSC ENDING", m)
  inicioSC <- grepl("SAFETY CAR DEPLOYED", m) & !inicioVSC
  finSC <- grepl("SAFETY CAR IN THIS LAP", m)
  corte <- bandera %in% c("RED", "CHEQUERED")
  filas <- list()
  abierto <- NULL
  cerrar <- function(i, como, convertido = FALSE) {
    a <- abierto
    filas[[length(filas) + 1]] <<- data.frame(
      tipo = a$tipo, vuelta_inicio = a$vuelta, vuelta_fin = control$lap_number[i],
      vueltas = control$lap_number[i] - a$vuelta + 1, desde = a$desde, hasta = fecha[i],
      segundos = as.numeric(difftime(fecha[i], a$desde, units = "secs")), cierre = como,
      convertido = convertido)
    abierto <<- NULL
  }
  for (i in seq_along(m)) {
    if (inicioVSC[i] && is.null(abierto)) {
      abierto <- list(tipo = "VSC", vuelta = control$lap_number[i], desde = fecha[i])
    } else if (inicioSC[i]) {
      if (!is.null(abierto) && abierto$tipo == "VSC") cerrar(i, "conversion_a_SC", TRUE)
      if (is.null(abierto)) abierto <- list(tipo = "SC", vuelta = control$lap_number[i], desde = fecha[i])
    } else if (!is.null(abierto) && ((abierto$tipo == "VSC" && finVSC[i]) || (abierto$tipo == "SC" && finSC[i]))) {
      cerrar(i, "mensaje_de_fin")
    } else if (!is.null(abierto) && corte[i]) {
      cerrar(i, if (bandera[i] == "RED") "bandera_roja" else "fin_de_carrera")
    }
  }
  if (!is.null(abierto)) {
    filas[[length(filas) + 1]] <- data.frame(
      tipo = abierto$tipo, vuelta_inicio = abierto$vuelta, vuelta_fin = NA_real_, vueltas = NA_real_,
      desde = abierto$desde, hasta = as.POSIXct(NA, tz = "UTC"), segundos = NA_real_,
      cierre = "sin_cierre", convertido = FALSE)
  }
  if (length(filas) == 0) return(vacio)
  do.call(rbind, filas)
}

# Probabilidad de que una carrera tenga al menos un episodio de cada tipo en un circuito,
# a partir de carreras secas ANTERIORES (previas: data.frame con circuito_key, sc y vsc
# logicos). La frecuencia del circuito se acerca a la tasa general con k carreras
# "ficticias" a la tasa general: con FastF1 2019-2026 las diferencias entre circuitos no
# fueron significativas (p = 0,13 SC; 0,77 VSC) y el mejor pronostico fue k = 8 para SC y
# la tasa general (k infinito) para VSC (resultados/calibrarProbabilidadesFF1.R).
# Devuelve tambien los cuatro escenarios suponiendo independencia entre SC y VSC.
probabilidadNeutralizacion <- function(previas, circuito, k = c(SC = 8, VSC = Inf)) {
  circ <- previas[previas$circuito_key %in% circuito, ]
  p <- c()
  for (t in c("SC", "VSC")) {
    col <- tolower(t); g <- mean(previas[[col]])
    p[t] <- if (is.infinite(k[[t]]) || nrow(circ) == 0) g else
      (sum(circ[[col]]) + k[[t]] * g) / (nrow(circ) + k[[t]])
  }
  list(p = p, ediciones_circuito = nrow(circ), carreras = nrow(previas),
       frecuencia_circuito = c(SC = if (nrow(circ)) mean(circ$sc) else NA,
                               VSC = if (nrow(circ)) mean(circ$vsc) else NA),
       escenarios = c(ninguno = (1 - p[["SC"]]) * (1 - p[["VSC"]]),
                      solo_VSC = (1 - p[["SC"]]) * p[["VSC"]],
                      solo_SC = p[["SC"]] * (1 - p[["VSC"]]),
                      ambos = p[["SC"]] * p[["VSC"]]))
}

# Distribucion del inicio de los episodios en 5 tramos de 20 % de la carrera, por tipo,
# con todas las carreras juntas (episodios: tipo, vuelta_inicio, vueltas_carrera).
momentoNeutralizacion <- function(episodios, tramos = 5) {
  t <- pmin(tramos, floor(tramos * (episodios$vuelta_inicio - 1) / episodios$vueltas_carrera) + 1)
  sapply(c("SC", "VSC"), function(x) {
    n <- tabulate(t[episodios$tipo == x], tramos)
    n / max(sum(n), 1)
  })
}

# Mejor forma de completar la carrera desde un estado: faltan R vueltas, el auto lleva el
# compuesto 'actual' con edad 'edad' (al empezar la proxima vuelta), ya hizo 'hechas'
# paradas y uso los compuestos 'usados'.
# - parar = TRUE: para AHORA pagando perdidaAhora y elige el compuesto siguiente.
# - parar = FALSE: sigue con el juego actual (al menos 1 vuelta) y puede parar despues.
# Las paradas posteriores pagan la perdida en verde. Restricciones: maxParadas en total,
# al menos dos compuestos secos en la carrera, stints nuevos de al menos minVueltasStint,
# vidaMaxima y maxFactorEdad como en generarEstrategias().
# Devuelve el tiempo (relativo, sin combustible: es igual para todas las opciones) y el plan.
mejorContinuacion <- function(R, actual, edad, hechas, usados, parar, modelo, perdidaVerde,
                              perdidaAhora = 0, maxParadas = 2, minVueltasStint = 5,
                              edadInicial = c(SOFT = 0, MEDIUM = 0, HARD = 0),
                              maxFactorEdad = 3, vidaMaxima = NULL) {
  disponibles <- modelo$compound[modelo$suficiente]
  libres <- maxParadas - hechas - as.integer(parar)
  if (libres < 0 || R < 1) return(list(tiempo = Inf))
  mejor <- list(tiempo = Inf)
  for (extra in 0:libres) {
    nStints <- extra + 1
    # Largos posibles: el primero >= 1 si sigue con el juego actual; los nuevos >= minimo.
    min1 <- if (parar) minVueltasStint else 1
    if (R < min1 + extra * minVueltasStint) next
    if (extra == 0) {
      largos <- matrix(R, ncol = 1)
    } else if (extra == 1) {
      a <- seq.int(min1, R - minVueltasStint); largos <- cbind(a, R - a)
    } else {
      g <- expand.grid(a = seq.int(min1, R - 2 * minVueltasStint), b = seq.int(minVueltasStint, R - min1 - minVueltasStint))
      g <- g[R - g$a - g$b >= minVueltasStint, ]
      if (nrow(g) == 0) next
      largos <- cbind(g$a, g$b, R - g$a - g$b)
    }
    primeros <- if (parar) disponibles else actual
    secuencias <- expand.grid(c(list(primeros), rep(list(disponibles), extra)), stringsAsFactors = FALSE)
    for (s in seq_len(nrow(secuencias))) {
      cpd <- unlist(secuencias[s, ])
      if (length(unique(c(usados, cpd))) < 2) next
      total <- perdidaAhora + extra * perdidaVerde
      ok <- rep(TRUE, nrow(largos))
      for (j in seq_along(cpd)) {
        m <- modelo[modelo$compound == cpd[j], ]
        if (nrow(m) != 1 || !m$suficiente) { ok[] <- FALSE; break }
        a0 <- if (j == 1 && !parar) edad else edadInicial[[cpd[j]]]
        cd <- caidaDe(m)
        total <- total + tiempoStint(m$base, m$pendiente_usada, a0, largos[, j], cd[1], cd[2])
        ok <- ok & (a0 + largos[, j] - 1 <= maxFactorEdad * m$edad_max_observada)
        if (!is.null(vidaMaxima)) ok <- ok & (a0 + largos[, j] <= vidaMaxima[[cpd[j]]])
      }
      if (!any(ok)) next
      i <- which(ok)[which.min(total[ok])]
      if (total[i] < mejor$tiempo) {
        mejor <- list(tiempo = total[i], compuestos = cpd, largos = largos[i, ],
                      paradas_restantes = extra, siguiente = if (parar) cpd[1] else NA_character_)
      }
    }
  }
  mejor
}

# Estado del auto al terminar la vuelta L siguiendo una estrategia (compuestos y vueltas
# de parada): compuesto actual, edad al empezar la vuelta L + 1, paradas hechas y usados.
estadoEnVuelta <- function(compuestos, vueltasParada, L, edadInicial) {
  fines <- c(vueltasParada, Inf)
  j <- which(L <= fines)[1]
  inicio <- if (j == 1) 0 else vueltasParada[j - 1]
  list(actual = compuestos[j], edad = edadInicial[[compuestos[j]]] + (L - inicio),
       hechas = j - 1, usados = unique(compuestos[seq_len(j)]))
}

# Decision ante una neutralizacion que empieza en la vuelta L (el auto puede entrar a
# boxes al final de esa vuelta), para un plan base. Compara parar ahora (pagando
# factorPerdida x perdida verde) contra seguir con el mejor plan desde ese estado.
# ahorro > 0: conviene parar. Supuestos declarados: el tiempo bajo neutralizacion es
# igual para ambas opciones; no se modela la menor degradacion bajo neutralizacion ni la
# perdida de posiciones; el combustible es igual para todas las opciones.
decisionNeutralizacion <- function(plan, L, tipo, nVueltas, modelo, perdidaVerde, factorPerdida,
                                   maxParadas = 2, minVueltasStint = 5,
                                   edadInicial = c(SOFT = 0, MEDIUM = 0, HARD = 0),
                                   maxFactorEdad = 3, vidaMaxima = NULL) {
  e <- estadoEnVuelta(plan$compuestos, plan$vueltasParada, L, edadInicial)
  R <- nVueltas - L
  args <- list(R = R, actual = e$actual, edad = e$edad, hechas = e$hechas, usados = e$usados,
               modelo = modelo, perdidaVerde = perdidaVerde, maxParadas = maxParadas,
               minVueltasStint = minVueltasStint, edadInicial = edadInicial,
               maxFactorEdad = maxFactorEdad, vidaMaxima = vidaMaxima)
  seguir <- do.call(mejorContinuacion, c(args, parar = FALSE))
  parar <- do.call(mejorContinuacion, c(args, parar = TRUE, perdidaAhora = factorPerdida[[tipo]] * perdidaVerde))
  # Si no se puede parar (quedan menos vueltas que un stint minimo) el ahorro queda en NA;
  # si no se puede seguir (falta parar para usar dos compuestos) conviene parar.
  ahorro <- if (!is.finite(parar$tiempo)) NA_real_ else if (!is.finite(seguir$tiempo)) Inf else
    seguir$tiempo - parar$tiempo
  # Sin ninguna opcion factible (ni parar ni seguir) se informa como tal.
  vacio <- function(x) if (is.null(x)) NA else x
  parar <- lapply(parar, vacio); seguir <- lapply(seguir, vacio)
  for (k in c("compuestos", "largos", "paradas_restantes", "siguiente")) {
    if (is.null(parar[[k]])) parar[[k]] <- NA
    if (is.null(seguir[[k]])) seguir[[k]] <- NA
  }
  data.frame(vuelta = L, tipo = tipo, accion = if (isTRUE(ahorro > 0)) "parar" else "seguir",
             ahorro = ahorro, siguiente = if (isTRUE(ahorro > 0)) parar$siguiente else NA_character_,
             paradas_restantes = if (isTRUE(ahorro > 0)) parar$paradas_restantes else seguir$paradas_restantes,
             plan_resto = if (isTRUE(ahorro > 0)) paste(parar$compuestos, collapse = "-") else paste(seguir$compuestos, collapse = "-"),
             largos_resto = if (isTRUE(ahorro > 0)) paste(parar$largos, collapse = "+") else paste(seguir$largos, collapse = "+"),
             tiempo_plan_fijo = NA_real_)
}

# Ventanas: decisiones vuelta por vuelta agrupadas (a) en vueltas consecutivas con la
# misma respuesta y (b) en 10 secciones de la carrera (respuesta mayoritaria).
ventanasNeutralizacion <- function(decisiones, nVueltas, secciones = 10) {
  out <- list()
  for (t in unique(decisiones$tipo)) {
    d <- decisiones[decisiones$tipo == t, ]; d <- d[order(d$vuelta), ]
    clave <- paste(d$accion, d$siguiente, d$paradas_restantes)
    grupo <- cumsum(c(TRUE, clave[-1] != clave[-length(clave)]))
    for (g in unique(grupo)) {
      x <- d[grupo == g, ]
      out[[length(out) + 1]] <- data.frame(tipo = t, desde = min(x$vuelta), hasta = max(x$vuelta),
        accion = x$accion[1], siguiente = x$siguiente[1], paradas_restantes = x$paradas_restantes[1],
        ahorro_mediano = suppressWarnings(median(x$ahorro[is.finite(x$ahorro)])))
    }
  }
  consecutivas <- do.call(rbind, out)
  decisiones$seccion <- pmin(secciones, floor(secciones * (decisiones$vuelta - 1) / nVueltas) + 1)
  porSeccion <- do.call(rbind, lapply(split(decisiones, list(decisiones$tipo, decisiones$seccion), drop = TRUE), function(x) {
    acc <- names(sort(table(x$accion), decreasing = TRUE))[1]
    sig <- x$siguiente[x$accion == acc]
    data.frame(tipo = x$tipo[1], seccion = x$seccion[1], desde = min(x$vuelta), hasta = max(x$vuelta),
               accion = acc, vueltas_con_esa_respuesta = sum(x$accion == acc), vueltas = nrow(x),
               siguiente = if (acc == "parar") names(sort(table(sig), decreasing = TRUE))[1] else NA_character_,
               ahorro_mediano = if (any(is.finite(x$ahorro))) median(x$ahorro[is.finite(x$ahorro)]) else NA_real_)
  }))
  porSeccion <- porSeccion[order(porSeccion$tipo, porSeccion$seccion), ]
  rownames(porSeccion) <- NULL
  list(consecutivas = consecutivas, secciones = porSeccion)
}

# Medida D: diferencia esperada (s) entre el plan fijo y la respuesta adaptativa.
# Escenario: un episodio del tipo t empieza en la vuelta L. Plan fijo: sigue igual; si una
# de sus paradas cae dentro del episodio (duracion tipica en vueltas), esa parada paga la
# perdida neutralizada. Adaptativo: el mejor entre ese plan fijo y parar en L.
# D(L, t) = fijo - adaptativo >= 0. Se pondera por la probabilidad del tipo en el circuito
# y por el momento (5 tramos, repartido en partes iguales dentro de cada tramo).
# Solo considera el primer episodio de cada tipo; no es una prueba de ventaja real.
medidaD <- function(plan, decisiones, nVueltas, perdidaVerde, factorPerdida, probabilidades,
                    momento, duracion = c(SC = 4, VSC = 2)) {
  filas <- list()
  for (t in unique(decisiones$tipo)) {
    d <- decisiones[decisiones$tipo == t, ]
    for (i in seq_len(nrow(d))) {
      L <- d$vuelta[i]
      enEpisodio <- plan$vueltasParada >= L & plan$vueltasParada <= L + duracion[[t]] - 1
      descuentoFijo <- sum(enEpisodio) * (1 - factorPerdida[[t]]) * perdidaVerde
      # El ahorro de parar se mide contra seguir sin descuento; si el plan fijo ya para en
      # el episodio, ese descuento se descuenta del beneficio de adaptarse.
      D <- if (is.finite(d$ahorro[i])) max(0, d$ahorro[i] - descuentoFijo) else 0
      tramo <- pmin(nrow(momento), floor(nrow(momento) * (L - 1) / nVueltas) + 1)
      vueltasTramo <- sum(pmin(nrow(momento), floor(nrow(momento) * (d$vuelta - 1) / nVueltas) + 1) == tramo)
      filas[[length(filas) + 1]] <- data.frame(tipo = t, vuelta = L, D = D,
        peso = probabilidades[[t]] * momento[tramo, t] / vueltasTramo)
    }
  }
  x <- do.call(rbind, filas)
  list(porVuelta = x, esperado = sum(x$D * x$peso),
       porTipo = tapply(x$D * x$peso, x$tipo, sum),
       condicional = tapply(seq_len(nrow(x)), x$tipo, function(i) sum(x$D[i] * x$peso[i]) / sum(x$peso[i])))
}
