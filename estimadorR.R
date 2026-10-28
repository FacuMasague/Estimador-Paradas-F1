library(DBI)
library(RSQLite)

prepararLluvia <- function(clima) {
  # Comprobar las columnas que necesitan esta funcion.
  if (!all(c("fechaUTC", "rainfall") %in% names(clima))) {
    stop("Faltan las columnas fechaUTC o rainfall.")
  }
  
  # Las fechas deben estar convertidas antes de llamar a esta funcion.
  if (
    !inherits(clima$fechaUTC, "POSIXct") ||
    anyNA(clima$fechaUTC)
  ) {
    stop("Las fechas deben estar convertidas y ser validas.")
  }
  
  # Obtener los instantes distintos, ordenados cronologicamente.
  instantes <- sort(unique(clima$fechaUTC))
  
  lluvia <- rep(NA_real_, length(instantes))
  registrosOriginales <- integer(length(instantes))
  for (i in seq_along(instantes)) {
    valores <- clima$rainfall[
      clima$fechaUTC == instantes[i]
    ]
    
    registrosOriginales[i] <- length(valores)
    
    # Aceptar el valor solo si todos son validos y coinciden.
    if (
      all(valores %in% c(0, 1)) &&
      length(unique(valores)) == 1
    ) {
      lluvia[i] <- valores[1]
    }
  }
  
  return(data.frame(
    fechaUTC = instantes,
    rainfall = lluvia,
    registros_originales = registrosOriginales
  ))
}

# Evaluar el tramo inicial candidato de UNA practica segun la lluvia observada.
# Entradas:
# - lluviaPreparada: tabla devuelta por prepararLluvia(), sin recortar al horario
#   de la practica; las observaciones anteriores y posteriores ayudan en los limites.
# - inicio y fin: instantes POSIXct correspondientes al horario de la practica.
# - maxHuecoSegundos: separacion maxima admitida entre observaciones, en segundos.
#   Los 120 segundos son un supuesto configurable, no un umbral validado.
# Salida: una fila con estado, motivo, limites del tramo candidato y primera lluvia.
# Estados: candidato_completo, candidato_parcial, descartar_inicio_con_lluvia
# o indeterminado. Un candidato indica cobertura sin lluvia observada;
# no demuestra que el asfalto estuviera seco ni que las vueltas sean utilizables.
# Antes de llamar a esta funcion se debe comprobar la descarga meteorologica.
evaluarLluvia <- function(lluviaPreparada, inicio, fin, maxHuecoSegundos = 120) {
  # 1. Convertir los instantes a segundos para calcular separaciones.
  a <- as.numeric(inicio)
  b <- as.numeric(fin)
  
  sinFecha <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )
  primeraLluvia <- sinFecha
  
  # 2. Definir una funcion auxiliar para devolver siempre las mismas columnas.
  # Solo conservar limites si existe un tramo de duracion positiva desde el inicio.
  resultado <- function(estado, motivo, corte = NA_real_) {
    hayTramo <- length(a) == 1 && is.finite(a) && length(corte) == 1 &&
      is.finite(corte) && corte > a
    
    if (estado == "candidato_parcial" && !hayTramo) {
      estado <- "indeterminado"
    }
    
    data.frame(
      estado = estado,
      motivo = motivo,
      inicio_candidato = if (hayTramo) {
        as.POSIXct(a, origin = "1970-01-01", tz = "UTC")
      } else {
        sinFecha
      },
      fin_candidato = if (hayTramo) {
        as.POSIXct(corte, origin = "1970-01-01", tz = "UTC")
      } else {
        sinFecha
      },
      primera_lluvia_observada = primeraLluvia
    )
  }
  
  # 3. Validar el limite de huecos, los horarios y la tabla recibida.
  if (
    length(maxHuecoSegundos) != 1 ||
    !is.finite(maxHuecoSegundos) ||
    maxHuecoSegundos <= 0
  ) {
    stop("El limite de huecos debe ser un numero positivo.")
  }
  
  if (
    length(a) != 1 || length(b) != 1 ||
    !is.finite(a) || !is.finite(b) || b <= a
  ) {
    return(resultado("indeterminado", "fechas_sesion_invalidas"))
  }
  
  if (!all(c("fechaUTC", "rainfall") %in% names(lluviaPreparada))) {
    stop("Faltan las columnas fechaUTC o rainfall")
  }
  
  if (nrow(lluviaPreparada) == 0) {
    return(resultado("indeterminado", "sin_observaciones"))
  }
  
  t <- as.numeric(lluviaPreparada$fechaUTC)
  r <- lluviaPreparada$rainfall
  
  if (any(!is.finite(t))) {
    return(resultado("indeterminado", "fecha_meteorologica_invalida"))
  }
  
  if (anyDuplicated(t) > 0) {
    return(resultado("indeterminado", "fechas_sin_agrupar"))
  }
  
  # Mantener alineados los instantes y sus valores de lluvia al ordenar.
  orden <- order(t)
  t <- t[orden]
  r <- r[orden]
  
  # 4. Registrar la primera lluvia observada dentro de la practica.
  # Es un dato informativo: el comienzo real pudo ocurrir entre dos mediciones.
  conLluvia <- which(t >= a & t <= b & r %in% 1)
  
  if (length(conLluvia) > 0) {
    primeraLluvia <- as.POSIXct(
      t[conLluvia[1]],
      origin = "1970-01-01",
      tz = "UTC"
    )
  }
  
  # 5. Buscar la ultima referencia tomada al inicio o antes del inicio.
  # Si falta, es antigua o tiene lluvia desconocida, no asumir un comienzo seco.
  previos <- which(t <= a)
  
  if (length(previos) == 0) {
    return(resultado("indeterminado", "sin_observacion_al_inicio"))
  }
  
  k <- tail(previos, 1)
  
  if (a - t[k] > maxHuecoSegundos) {
    return(resultado("indeterminado", "observacion_inicial_antigua"))
  }
  
  if (!r[k] %in% c(0, 1)) {
    return(resultado("indeterminado", "lluvia_inicial_desconocida_o_contradictoria"))
  }
  
  # Simplificacion acordada: descartar si la referencia inicial indica lluvia,
  # aunque mas adelante deje de llover.
  if (r[k] == 1) {
    return(resultado("descartar_inicio_con_lluvia", "lluvia_en_referencia_inicial"))
  }
  
  # Guardar la ultima observacion sin lluvia conectada con el inicio.
  # El nombre no implica una medicion del estado seco del asfalto.
  ultimoSeco <- t[k]
  
  # 6. Recorrer las observaciones hasta encontrar el primer limite del tramo.
  if (k < length(t)) {
    for (j in seq.int(k + 1, length(t))) {
      # Una lectura posterior al fin, sin lluvia y suficientemente cercana,
      # permite cubrir el cierre bajo el supuesto del hueco maximo.
      # Si esa lectura tiene lluvia, no atribuirla retrospectivamente a la practica.
      if (t[j] > b) {
        if (r[j] %in% 0 && t[j] - ultimoSeco <= maxHuecoSegundos) {
          return(resultado("candidato_completo",
 "sin_lluvia_observada_con_cobertura",
                           b))
        }
        
        return(resultado("candidato_parcial",
                         "final_sin_cobertura_suficiente",
                         ultimoSeco))
      }
      
      # Un hueco grande interrumpe la cobertura: conservar solo el tramo anterior.
      if (t[j] - ultimoSeco > maxHuecoSegundos) {
        return(resultado("candidato_parcial",
                         "hueco_en_observaciones",
                         ultimoSeco))
      }
      
      # Un valor desconocido o contradictorio tambien interrumpe la cobertura.
      if (!r[j] %in% c(0, 1)) {
        return(resultado("candidato_parcial",
                         "lluvia_desconocida_o_contradictoria",
                         ultimoSeco))
      }
      
      # Ante lluvia, cortar en la ultima observacion sin lluvia, no en la primera
      # con lluvia: no conocemos el instante exacto en que empezo a llover.
      if (r[j] == 1) {
        return(resultado("candidato_parcial",
                         "primer_registro_de_lluvia",
                         ultimoSeco))
      }
      
      # Esta observacion tiene rainfall = 0 y mantiene la continuidad del tramo.
      ultimoSeco <- t[j]
      
      if (ultimoSeco == b) {
        return(resultado("candidato_completo",
                         "sin_lluvia_observada_con_cobertura",
                         b))
      }
    }
  }
  
  # 7. Si se agotaron las observaciones antes de cubrir el final, devolver
  # solamente el tramo respaldado. Sin duracion positiva queda indeterminado.
  return(resultado("candidato_parcial",
                   "final_sin_cobertura_suficiente",
                   ultimoSeco))
}

# Revisar la informacion meteorologica de UNA practica.
# practica: una fila de practicasCandidatas.
# clima: tabla de observaciones, con session_key.
# descargas: tabla de estados de descarga, con session_key.
# maxHuecoSegundos: limite configurable usado por evaluarLluvia().
revisarClimaPractica <- function(
  practica,
  clima,
  descargas,
  maxHuecoSegundos = 120
) {
  # 1. Comprobar que se recibio exactamente una practica.
  if (nrow(practica) != 1) {
    stop("Se debe recibir exactamente una practica.")
  }

  # Preparar un valor de fecha desconocida, conservando el tipo POSIXct.
  sinFecha <- as.POSIXct(
    NA_real_,
    origin = "1970-01-01",
    tz = "UTC"
  )

  # Devolver siempre las mismas columnas cuando falte informacion.
  # Esto permite continuar con las otras practicas mas adelante.
  indeterminado <- function(motivo) {
    data.frame(
      estado = "indeterminado",
      motivo = motivo,
      inicio_candidato = sinFecha,
      fin_candidato = sinFecha,
      primera_lluvia_observada = sinFecha
    )
  }

  # 2. Buscar la descarga y las observaciones de esta sesion.
  clave <- practica$session_key

  descarga <- descargas[
    descargas$session_key %in% clave,
  ]

  observaciones <- clima[
    clima$session_key %in% clave,
  ]

  # 3. Comprobar que exista un unico registro de descarga exitoso.
  if (nrow(descarga) == 0) {
    return(indeterminado("sin_registro_de_descarga"))
  }

  if (nrow(descarga) != 1) {
    return(indeterminado("registros_de_descarga_ambiguos"))
  }

  if (!isTRUE(descarga$estado[1] == "ok")) {
    return(indeterminado("descarga_no_ok"))
  }

  # 4. Comparar las filas guardadas con la cantidad registrada por Python.
  # Hacerlo antes de agrupar observaciones del mismo instante.
  if (!isTRUE(descarga$filas[1] == nrow(observaciones))) {
    return(indeterminado("cantidad_de_filas_inconsistente"))
  }

  if (nrow(observaciones) == 0) {
    return(indeterminado("sin_observaciones"))
  }

  # 5. Convertir y comprobar los horarios de la practica.
  # Las fechas de esta base estan expresadas en UTC.
  inicio <- as.POSIXct(
    practica$fecha_inicio,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )

  fin <- as.POSIXct(
    practica$fecha_fin,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )

  if (anyNA(inicio) || anyNA(fin) || fin <= inicio) {
    return(indeterminado("fechas_sesion_invalidas"))
  }

  # 6. Convertir las fechas de las observaciones meteorologicas.
  observaciones$fechaUTC <- as.POSIXct(
    observaciones$date,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )

  if (anyNA(observaciones$fechaUTC)) {
    return(indeterminado("fecha_meteorologica_invalida"))
  }

  # 7. Agrupar las observaciones y evaluar la lluvia.
  # Conservar tambien las lecturas anteriores y posteriores a la practica.
  preparada <- prepararLluvia(observaciones)

  return(evaluarLluvia(
    lluviaPreparada = preparada,
    inicio = inicio,
    fin = fin,
    maxHuecoSegundos = maxHuecoSegundos
  ))
}

# Mostrar un menu y devolver el numero de opcion elegido.
# El cero permite cancelar. leer se puede sustituir al comprobar la funcion
# para ensayar respuestas sin tener que escribirlas manualmente.
elegirOpcion <- function(titulo, opciones, leer = readline) {
  if (length(opciones) == 0) {
    stop("No hay opciones disponibles para este menu.")
  }

  cat("\n", titulo, "\n", sep = "")
  for (i in seq_along(opciones)) {
    cat(i, ") ", opciones[i], "\n", sep = "")
  }
  cat("0) Cancelar\n")

  repeat {
    respuesta <- trimws(leer("Elegi el numero de una opcion: "))

    # Aceptar solamente numeros enteros dentro del rango mostrado.
    if (length(respuesta) == 1 && !is.na(respuesta) &&
        grepl("^[0-9]+$", respuesta)) {
      numero <- as.numeric(respuesta)
      if (is.finite(numero) && numero >= 0 && numero <= length(opciones)) {
        return(as.integer(numero))
      }
    }

    cat("Entrada invalida. Usa un numero entre 0 y", length(opciones), ".\n")
  }
}

# Leer los pilotos registrados en las practicas candidatas seleccionadas.
# Solo usar descargas exitosas cuya cantidad de filas coincida con la tabla.
# Esto verifica el catalogo de pilotos, no la suficiencia de sus vueltas.
leerPilotosPracticas <- function(rutaBase, clavesSesiones) {
  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(conexion), add = TRUE)

  pilotos <- data.frame(driver_number = integer(), full_name = character())

  for (clave in unique(clavesSesiones)) {
    descarga <- dbGetQuery(
      conexion,
      "SELECT estado, filas FROM descargas
       WHERE endpoint = 'drivers'
       AND json_extract(parametros_json, '$.session_key') = ?",
      params = list(clave)
    )
    registros <- dbGetQuery(
      conexion,
      "SELECT driver_number, full_name FROM pilotos WHERE session_key = ?",
      params = list(clave)
    )

    if (nrow(descarga) != 1 || !isTRUE(descarga$estado[1] == "ok") ||
        !isTRUE(descarga$filas[1] == nrow(registros))) {
      warning(paste("Se omiten los pilotos de la sesion", clave,
                    "porque su descarga debe revisarse."), call. = FALSE)
      next
    }

    pilotos <- rbind(pilotos, registros)
  }

  # Un piloto puede figurar en varias practicas: mostrarlo una sola vez.
  # No unir silenciosamente nombres distintos asociados al mismo dorsal.
  opciones <- data.frame(driver_number = integer(), full_name = character())
  for (dorsal in sort(unique(pilotos$driver_number[!is.na(pilotos$driver_number)]))) {
    nombres <- trimws(pilotos$full_name[pilotos$driver_number %in% dorsal])
    nombres <- unique(nombres[!is.na(nombres) & nzchar(nombres)])

    if (length(nombres) > 1) {
      warning(paste("Se omite el dorsal", dorsal,
                    "porque tiene nombres contradictorios."), call. = FALSE)
      next
    }

    nombre <- if (length(nombres) == 1) nombres else "Nombre no disponible"
    opciones <- rbind(opciones, data.frame(
      driver_number = dorsal,
      full_name = nombre
    ))
  }

  return(opciones)
}

# Elegir temporada, GP y piloto sin descargar datos ni modificar SQLite.
# Devolver las filas elegidas y los tramos candidatos para el analisis posterior.
seleccionarAnalisis <- function(gpDisponibles, revisionClima, rutaBase,
                               leer = readline) {
  if (nrow(gpDisponibles) == 0) {
    cat("No hay Grandes Premios con tramos candidatos.\n")
    return(NULL)
  }

  temporadas <- sort(unique(gpDisponibles$temporada), decreasing = TRUE)
  opcion <- elegirOpcion("Selecciona la temporada", as.character(temporadas), leer)
  if (opcion == 0) return(NULL)
  temporada <- temporadas[opcion]

  opcionesGP <- gpDisponibles[gpDisponibles$temporada %in% temporada, ]
  opcionesGP <- opcionesGP[order(opcionesGP$meeting_key), ]
  etiquetasGP <- paste0(
    opcionesGP$nombre, " (", opcionesGP$practicas_completas,
    " practicas completas, ", opcionesGP$practicas_parciales, " parciales)"
  )
  opcion <- elegirOpcion("Selecciona el Gran Premio", etiquetasGP, leer)
  if (opcion == 0) return(NULL)
  gp <- opcionesGP[opcion, , drop = FALSE]

  tramos <- revisionClima[
    revisionClima$meeting_key %in% gp$meeting_key &
      revisionClima$estado %in% c("candidato_completo", "candidato_parcial"),
  ]

  pilotos <- leerPilotosPracticas(rutaBase, tramos$session_key)
  if (nrow(pilotos) == 0) {
    cat("No hay pilotos disponibles con registros verificables en estas practicas.\n")
    return(NULL)
  }

  cat("\nLa lista de pilotos es preliminar: falta comprobar sus vueltas y compuestos.\n")
  etiquetasPilotos <- paste0(pilotos$full_name, " (dorsal ", pilotos$driver_number, ")")
  opcion <- elegirOpcion("Selecciona el piloto", etiquetasPilotos, leer)
  if (opcion == 0) return(NULL)

  return(list(
    temporada = temporada,
    granPremio = gp,
    piloto = pilotos[opcion, , drop = FALSE],
    tramos = tramos
  ))
}

# Anotar las vueltas originales sin borrar registros ni renumerar su uso.
# date_start es aproximado segun OpenF1. El final calculado tambien lo es.
# lap_duration se expresa en segundos. edad_inicio se expresa en vueltas completadas.
anotarVueltasPiloto <- function(vueltas, stints, inicio, fin) {
  vueltas$inicioUTC <- as.POSIXct(
    vueltas$date_start, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
  )
  duracionValida <- is.finite(vueltas$lap_duration) & vueltas$lap_duration > 0
  vueltas$finUTC <- vueltas$inicioUTC + ifelse(duracionValida, vueltas$lap_duration, NA_real_)

  # Una vuelta solo pasa el filtro horario si su inicio Y su final estan dentro.
  vueltas$estado_horario <- rep("fecha_o_duracion_invalida", nrow(vueltas))
  fechasValidas <- !is.na(vueltas$inicioUTC) & !is.na(vueltas$finUTC)
  if (length(inicio) == 1 && length(fin) == 1 &&
      !is.na(inicio) && !is.na(fin) && fin > inicio) {
    vueltas$estado_horario[fechasValidas] <- "fuera_del_tramo"
    dentro <- fechasValidas & vueltas$inicioUTC >= inicio & vueltas$finUTC <= fin
    vueltas$estado_horario[dentro %in% TRUE] <- "dentro_del_tramo"
  }

  # Los duplicados se conservan y se marcan para no usarlos inadvertidamente.
  claves <- vueltas[, c("session_key", "driver_number", "lap_number")]
  vueltas$numero_repetido <- duplicated(claves) | duplicated(claves, fromLast = TRUE)
  vueltas$estado_stint <- rep("sin_coincidencia", nrow(vueltas))
  vueltas$stint_number <- rep(NA_integer_, nrow(vueltas))
  vueltas$compound <- rep(NA_character_, nrow(vueltas))
  vueltas$lap_start_stint <- rep(NA_integer_, nrow(vueltas))
  vueltas$tyre_age_at_start <- rep(NA_integer_, nrow(vueltas))

  for (i in seq_len(nrow(vueltas))) {
    numero <- vueltas$lap_number[i]
    if (!is.finite(numero) || numero < 1 || numero != floor(numero)) {
      vueltas$estado_stint[i] <- "numero_vuelta_invalido"
      next
    }

    # Cruzar por sesion, piloto y numero original de vuelta, nunca solo por vuelta.
    tandas <- stints[
      stints$session_key %in% vueltas$session_key[i] &
        stints$driver_number %in% vueltas$driver_number[i],
    ]
    if (nrow(tandas) == 0) next

    limitesValidos <- (
      is.finite(tandas$lap_start) & is.finite(tandas$lap_end) &
      tandas$lap_start >= 1 & tandas$lap_end >= tandas$lap_start &
      tandas$lap_start == floor(tandas$lap_start) &
      tandas$lap_end == floor(tandas$lap_end) &
      is.finite(tandas$stint_number) & tandas$stint_number >= 1 &
      tandas$stint_number == floor(tandas$stint_number)
    )
    # Sin limites completos no se puede descartar otra coincidencia posible.
    if (!all(limitesValidos)) {
      vueltas$estado_stint[i] <- "datos_stint_invalidos"
      next
    }

    coincidencias <- which(numero >= tandas$lap_start & numero <= tandas$lap_end)
    if (length(coincidencias) > 1) {
      vueltas$estado_stint[i] <- "ambiguo"
    } else if (length(coincidencias) == 1) {
      tanda <- tandas[coincidencias, ]
      vueltas$estado_stint[i] <- "asignado"
      vueltas$stint_number[i] <- tanda$stint_number
      vueltas$compound[i] <- tanda$compound
      vueltas$lap_start_stint[i] <- tanda$lap_start
      vueltas$tyre_age_at_start[i] <- tanda$tyre_age_at_start
    }
  }

  # Edad al INICIO de la vuelta: vueltas completadas por ese juego antes de esta.
  # Se calcula con el numero original de vuelta, no con las vueltas que sobreviven
  # a la limpieza: una vuelta descartada igual consumio uso del neumatico.
  # Sin stint unico asignado o sin edad inicial conocida queda NA.
  vueltas$edad_inicio <- vueltas$tyre_age_at_start +
    (vueltas$lap_number - vueltas$lap_start_stint)

  return(vueltas)
}

# Convertir fechas ISO de OpenF1 (UTC) a POSIXct, como en el resto del script.
aFechaUTC <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}

# Tabla de intervalos sin filas, con las mismas columnas que armarIntervalosControl().
intervalosVacios <- function() {
  sinFecha <- as.POSIXct(character(), tz = "UTC")
  data.frame(tipo = character(), desde = sinFecha, hasta = sinFecha,
             cerrado = logical())
}

# Construir intervalos de neutralizacion de UNA sesion a partir de race_control.
# Los mensajes son instantes: el intervalo va desde el mensaje que lo abre hasta el
# primer mensaje posterior que lo cierra. Sin cierre se extiende hasta finTramo y
# queda cerrado = FALSE, para no suponer que la situacion termino.
# Tipos: amarilla (solo informativa), bandera_roja y vsc_sc.
# El campo sector de race_control es un sector de comisarios, no uno de los tres
# sectores de cronometraje: por eso no se cruza con duration_sector_1..3.
armarIntervalosControl <- function(control, finTramo) {
  if (nrow(control) == 0) return(intervalosVacios())

  fecha <- aFechaUTC(control$date)
  orden <- order(fecha)
  control <- control[orden, ]
  fecha <- fecha[orden]
  bandera <- ifelse(is.na(control$flag), "", control$flag)
  mensaje <- toupper(ifelse(is.na(control$message), "", control$message))
  pista <- control$scope %in% "Track"

  filas <- list()
  for (i in seq_len(nrow(control))) {
    if (bandera[i] %in% c("YELLOW", "DOUBLE YELLOW")) {
      tipo <- "amarilla"
      # La cierra un CLEAR del mismo sector de comisarios o de toda la pista.
      cierra <- (bandera == "CLEAR" &
                   (control$sector %in% control$sector[i] | pista)) |
        bandera %in% c("RED", "CHEQUERED")
    } else if (bandera[i] == "RED" || mensaje[i] == "SESSION ABORTED") {
      tipo <- "bandera_roja"
      # La sesion se reanuda con la luz verde de salida de boxes.
      cierra <- (bandera == "GREEN" & pista) | mensaje == "SESSION STARTED" |
        bandera == "CHEQUERED"
    } else if (grepl("SAFETY CAR DEPLOYED|VSC DEPLOYED", mensaje[i])) {
      tipo <- "vsc_sc"
      cierra <- grepl("ENDING|IN THIS LAP", mensaje) |
        bandera %in% c("RED", "CHEQUERED")
    } else {
      next
    }

    siguiente <- which(seq_along(cierra) > i & cierra)[1]
    filas[[length(filas) + 1]] <- data.frame(
      tipo = tipo,
      desde = fecha[i],
      hasta = if (is.na(siguiente)) finTramo else fecha[siguiente],
      cerrado = !is.na(siguiente)
    )
  }

  if (length(filas) == 0) return(intervalosVacios())
  return(do.call(rbind, filas))
}

# Marcar vueltas identificables como no representativas, sin borrar registros.
# vueltas: vueltas de UNA sesion, ya anotadas por anotarVueltasPiloto().
# intervalos: salida de armarIntervalosControl() para esa misma sesion.
# controlVerificado: FALSE si la descarga de race_control no pudo comprobarse;
# en ese caso no se puede afirmar que no hubo neutralizaciones.
# Una vuelta marcada igual consumio uso del neumatico: no recalcula edades.
# candidata_ajuste NO significa vuelta limpia: faltan los residuos atipicos.
marcarVueltasNoRepresentativas <- function(vueltas, intervalos, controlVerificado) {
  n <- nrow(vueltas)
  vueltas$salida_boxes <- vueltas$is_pit_out_lap %in% 1
  vueltas$salida_boxes_desconocida <- is.na(vueltas$is_pit_out_lap)

  # Entrada a boxes INFERIDA: OpenF1 no trae un campo de entrada.
  # Se supone que la vuelta n termina en boxes si la n+1 del mismo piloto y
  # sesion es una vuelta de salida.
  vueltas$entrada_boxes_inferida <- rep(FALSE, n)
  for (i in seq_len(n)) {
    if (!is.finite(vueltas$lap_number[i])) next
    siguiente <- which(
      vueltas$session_key %in% vueltas$session_key[i] &
        vueltas$driver_number %in% vueltas$driver_number[i] &
        vueltas$lap_number %in% (vueltas$lap_number[i] + 1)
    )
    vueltas$entrada_boxes_inferida[i] <- any(vueltas$is_pit_out_lap[siguiente] %in% 1)
  }

  vueltas$duracion_invalida <- !(is.finite(vueltas$lap_duration) &
                                   vueltas$lap_duration > 0)

  # Una vuelta se considera afectada si se superpone con el intervalo,
  # aunque sea por un instante.
  conHorario <- !is.na(vueltas$inicioUTC) & !is.na(vueltas$finUTC)
  for (tipo in c("amarilla", "bandera_roja", "vsc_sc")) {
    vueltas[[tipo]] <- rep(FALSE, n)
  }
  for (k in seq_len(nrow(intervalos))) {
    toca <- conHorario & vueltas$inicioUTC <= intervalos$hasta[k] &
      vueltas$finUTC >= intervalos$desde[k]
    vueltas[[intervalos$tipo[k]]] <- vueltas[[intervalos$tipo[k]]] | (toca %in% TRUE)
  }
  # La amarilla es solo informativa: su efecto real lo detectan los residuos.
  names(vueltas)[names(vueltas) == "amarilla"] <- "con_amarilla"
  vueltas$control_no_verificable <- rep(!isTRUE(controlVerificado), n)

  # Una vuelta puede tener varios motivos, separados por punto y coma.
  motivos <- c("salida_boxes", "salida_boxes_desconocida", "entrada_boxes_inferida",
               "duracion_invalida", "bandera_roja", "vsc_sc",
               "control_no_verificable")
  vueltas$motivos_exclusion <- rep("", n)
  for (motivo in motivos) {
    m <- vueltas[[motivo]]
    vueltas$motivos_exclusion[m] <- ifelse(
      vueltas$motivos_exclusion[m] == "", motivo,
      paste(vueltas$motivos_exclusion[m], motivo, sep = ";")
    )
  }

  vueltas$candidata_ajuste <- vueltas$estado_horario == "dentro_del_tramo" &
    vueltas$estado_stint == "asignado" & !vueltas$numero_repetido &
    vueltas$motivos_exclusion == ""
  return(vueltas)
}

# Ajuste lineal robusto de Theil-Sen: tiempo = base + pendiente * edad.
# La pendiente es la mediana de las pendientes entre todos los pares de vueltas con
# edades distintas; la base es la mediana de (tiempo - pendiente * edad).
# A diferencia de lm(), una vuelta muy lenta no arrastra la recta: tolera hasta
# cerca de un 29 % de valores atipicos. Se usa solo para el ajuste PRELIMINAR
# que detecta residuos; no es el modelo final por compuesto.
# Unidades: tiempo en segundos, edad en vueltas, pendiente en segundos por vuelta.
ajustarTheilSen <- function(edad, tiempo) {
  pares <- combn(seq_along(edad), 2)
  i <- pares[1, ]
  j <- pares[2, ]
  distintas <- edad[i] != edad[j]
  if (!any(distintas)) stop("Se necesitan al menos dos edades distintas.")
  pendiente <- median((tiempo[j] - tiempo[i])[distintas] /
                        (edad[j] - edad[i])[distintas])
  base <- median(tiempo - pendiente * edad)
  list(base = base, pendiente = pendiente)
}

# Marcar residuos atipicos, lentos y rapidos, sin borrar registros.
# vueltas: vueltas del piloto en las TRES practicas, con candidata_ajuste y edad_inicio.
# Centro: ajuste preliminar robusto por stint (Theil-Sen, tiempo ~ edad). Escala: MAD escalada de
# los residuos de la practica; si no alcanza, de las tres practicas del piloto;
# si tampoco alcanza, no se evalua automaticamente.
# minVueltasStint, minResiduos y umbral son supuestos configurables, no validados.
# Que una vuelta no sea atipica no garantiza que no la afecto una amarilla leve.
marcarResiduosAtipicos <- function(vueltas, minVueltasStint = 4, minResiduos = 15,
                                   umbral = 3) {
  n <- nrow(vueltas)
  vueltas$residuo <- rep(NA_real_, n)
  vueltas$estado_residuo <- ifelse(vueltas$candidata_ajuste %in% TRUE,
                                   "pendiente", "no_candidata")
  vueltas$origen_escala <- rep(NA_character_, n)
  vueltas$residuo_lento <- rep(FALSE, n)
  vueltas$residuo_rapido <- rep(FALSE, n)

  # 1. Ajuste preliminar por stint. stint_number solo no identifica una tanda:
  #    la clave incluye sesion y piloto.
  candidatas <- vueltas$candidata_ajuste %in% TRUE
  clave <- paste(vueltas$session_key, vueltas$driver_number, vueltas$stint_number)
  for (k in unique(clave[candidatas])) {
    filas <- which(clave == k & candidatas)
    if (any(!is.finite(vueltas$edad_inicio[filas]))) {
      vueltas$estado_residuo[filas] <- "edad_desconocida"
      next
    }
    # Con pocas vueltas la recta queda forzada por los puntos y el residuo no
    # informa. Esto tambien deja afuera las simulaciones de clasificacion.
    if (length(filas) < minVueltasStint ||
        length(unique(vueltas$edad_inicio[filas])) < 2) {
      vueltas$estado_residuo[filas] <- "stint_corto"
      next
    }
    ajuste <- ajustarTheilSen(vueltas$edad_inicio[filas], vueltas$lap_duration[filas])
    vueltas$residuo[filas] <- vueltas$lap_duration[filas] -
      (ajuste$base + ajuste$pendiente * vueltas$edad_inicio[filas])
  }

  # 2. Escala robusta. Devuelve NULL con pocos residuos o con MAD = 0.
  # Los tiempos tienen resolucion de 0,001 s: una escala menor se trata como 0,
  # porque los residuos de un ajuste exacto no dan 0 exacto por redondeo numerico.
  conResiduo <- !is.na(vueltas$residuo)
  calcularEscala <- function(filas) {
    r <- vueltas$residuo[filas]
    if (length(r) < minResiduos) return(NULL)
    escala <- 1.4826 * median(abs(r - median(r)))
    if (!is.finite(escala) || escala < 0.001) return(NULL)
    list(centro = median(r), escala = escala)
  }
  respaldo <- calcularEscala(which(conResiduo))

  # 3. Evaluar cada practica con su escala o, si falta, con la de respaldo.
  for (sesion in unique(vueltas$session_key[conResiduo])) {
    filas <- which(conResiduo & vueltas$session_key %in% sesion)
    e <- calcularEscala(filas)
    origen <- "practica"
    if (is.null(e)) {
      e <- respaldo
      origen <- "tres_practicas"
    }
    if (is.null(e)) {
      vueltas$estado_residuo[filas] <- "no_evaluada_pocos_residuos"
      next
    }
    z <- (vueltas$residuo[filas] - e$centro) / e$escala
    vueltas$residuo_lento[filas] <- z > umbral
    vueltas$residuo_rapido[filas] <- z < -umbral
    vueltas$estado_residuo[filas] <- "evaluada"
    vueltas$origen_escala[filas] <- origen
  }

  # Apta para el ajuste: candidata, en un stint ajustable y no atipica.
  # Las no evaluadas por falta de residuos se conservan, con advertencia en el resumen.
  vueltas$apta_ajuste <- vueltas$estado_residuo %in%
    c("evaluada", "no_evaluada_pocos_residuos") &
    !vueltas$residuo_lento & !vueltas$residuo_rapido

  # 4. Un stint que conserva menos de minVueltasStint vueltas aptas no sostiene
  # una pendiente: por ejemplo, uno que alterna vueltas de empuje y de enfriamiento.
  # Se excluye completo, pero conserva sus marcas de residuo para poder revisarlo.
  for (k in unique(clave[vueltas$apta_ajuste])) {
    filas <- which(clave == k & vueltas$estado_residuo %in%
                     c("evaluada", "no_evaluada_pocos_residuos"))
    if (sum(vueltas$apta_ajuste[filas]) < minVueltasStint) {
      vueltas$estado_residuo[filas] <- "stint_insuficiente"
      vueltas$apta_ajuste[filas] <- FALSE
    }
  }
  return(vueltas)
}

# Revisar las tres practicas del GP seleccionado, sin mezclar sus stints.
# Una practica descartada por clima figura en el resumen pero no aporta vueltas.
# Un problema de descarga en una sesion no impide revisar las otras.
revisarPracticasPiloto <- function(seleccion, revisionClima, rutaBase) {
  practicas <- revisionClima[
    revisionClima$meeting_key %in% seleccion$granPremio$meeting_key,
  ]
  practicas <- practicas[order(practicas$nombre), ]
  if (nrow(practicas) != 3 || anyDuplicated(practicas$nombre) > 0) {
    stop("Se esperaban las tres practicas clasificadas del GP seleccionado.")
  }

  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(conexion), add = TRUE)
  dorsal <- seleccion$piloto$driver_number

  # Mantener las columnas originales, incluidos los identificadores de registro.
  vueltasVacias <- dbGetQuery(conexion, "SELECT * FROM vueltas LIMIT 0")
  stintsVacios <- dbGetQuery(conexion, "SELECT * FROM stints LIMIT 0")
  sinFecha <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "UTC")
  # La tabla vacia pasa por las mismas funciones para que rbind encuentre las
  # mismas columnas que en las sesiones revisadas.
  vueltasTodas <- marcarVueltasNoRepresentativas(
    anotarVueltasPiloto(vueltasVacias, stintsVacios, sinFecha, sinFecha),
    intervalosVacios(), TRUE
  )
  stintsTodos <- stintsVacios

  resumen <- practicas[, c("meeting_key", "session_key", "nombre", "estado", "motivo")]
  names(resumen)[4:5] <- c("estado_clima", "motivo_clima")
  resumen$estado_datos <- "no_revisado_por_clima"
  resumen$control_verificado <- NA
  for (columna in c("vueltas_registradas", "stints_registrados", "vueltas_en_tramo",
                    "vueltas_con_stint_unico", "vueltas_stint_ambiguo",
                    "vueltas_numero_repetido", "neutralizaciones_sin_cierre",
                    "vueltas_con_amarilla", "vueltas_candidatas_ajuste",
                    "vueltas_stint_corto_o_insuficiente", "vueltas_atipicas",
                    "vueltas_sin_evaluar_residuo", "vueltas_aptas_ajuste")) {
    resumen[[columna]] <- NA_integer_
  }

  for (i in seq_len(nrow(practicas))) {
    practica <- practicas[i, ]
    if (!practica$estado %in% c("candidato_completo", "candidato_parcial")) next

    parametros <- list(practica$session_key)
    vueltas <- dbGetQuery(conexion,
      "SELECT * FROM vueltas WHERE session_key = ? ORDER BY lap_number, registro",
      params = parametros)
    stints <- dbGetQuery(conexion,
      "SELECT * FROM stints WHERE session_key = ? ORDER BY stint_number, registro",
      params = parametros)
    descargas <- dbGetQuery(conexion,
      "SELECT endpoint, estado, filas FROM descargas
       WHERE endpoint IN ('laps', 'stints')
       AND json_extract(parametros_json, '$.session_key') = ?",
      params = parametros)

    if (nrow(descargas) != 2 || anyDuplicated(descargas$endpoint) > 0 ||
        !setequal(descargas$endpoint, c("laps", "stints")) ||
        !all(descargas$estado %in% "ok")) {
      resumen$estado_datos[i] <- "descargas_ausentes_o_no_ok"
      next
    }
    filas <- c(laps = nrow(vueltas), stints = nrow(stints))
    if (anyNA(descargas$filas) || !all(descargas$filas == filas[descargas$endpoint])) {
      resumen$estado_datos[i] <- "cantidad_de_filas_inconsistente"
      next
    }

    vueltas <- vueltas[vueltas$driver_number %in% dorsal, ]
    stints <- stints[stints$driver_number %in% dorsal, ]
    vueltas <- anotarVueltasPiloto(
      vueltas, stints, practica$inicio_candidato, practica$fin_candidato
    )

    # Control de carrera de la sesion COMPLETA: las banderas no tienen piloto.
    # Si la descarga no puede comprobarse, la sesion se revisa igual y sus vueltas
    # quedan marcadas como control_no_verificable.
    control <- dbGetQuery(conexion,
      "SELECT * FROM control_carrera WHERE session_key = ? ORDER BY date, registro",
      params = parametros)
    descargaControl <- dbGetQuery(conexion,
      "SELECT estado, filas FROM descargas
       WHERE endpoint = 'race_control'
       AND json_extract(parametros_json, '$.session_key') = ?",
      params = parametros)
    # Toda practica tiene al menos el mensaje SESSION STARTED: una descarga
    # vacia tampoco permite afirmar que no hubo neutralizaciones.
    controlVerificado <- nrow(descargaControl) == 1 &&
      isTRUE(descargaControl$estado[1] == "ok") &&
      isTRUE(descargaControl$filas[1] == nrow(control)) &&
      nrow(control) > 0 && !anyNA(aFechaUTC(control$date))
    intervalos <- if (controlVerificado) {
      armarIntervalosControl(control, practica$fin_candidato)
    } else {
      intervalosVacios()
    }
    vueltas <- marcarVueltasNoRepresentativas(vueltas, intervalos, controlVerificado)
    resumen$control_verificado[i] <- controlVerificado
    # Solo importan las que quedan abiertas dentro del tramo candidato.
    resumen$neutralizaciones_sin_cierre[i] <- sum(
      !intervalos$cerrado & intervalos$tipo != "amarilla" &
        intervalos$desde <= practica$fin_candidato
    )

    resumen$estado_datos[i] <- if (nrow(vueltas) == 0) {
      "sin_vueltas_del_piloto"
    } else if (nrow(stints) == 0) {
      "sin_stints_del_piloto"
    } else {
      "revisado"
    }
    resumen$vueltas_registradas[i] <- nrow(vueltas)
    resumen$stints_registrados[i] <- nrow(stints)
    dentro <- vueltas$estado_horario == "dentro_del_tramo"
    resumen$vueltas_en_tramo[i] <- sum(dentro)
    resumen$vueltas_con_stint_unico[i] <- sum(
      dentro & vueltas$estado_stint == "asignado" & !vueltas$numero_repetido
    )
    resumen$vueltas_stint_ambiguo[i] <- sum(dentro & vueltas$estado_stint == "ambiguo")
    resumen$vueltas_numero_repetido[i] <- sum(vueltas$numero_repetido)
    resumen$vueltas_con_amarilla[i] <- sum(dentro & vueltas$con_amarilla)
    resumen$vueltas_candidatas_ajuste[i] <- sum(vueltas$candidata_ajuste)
    vueltasTodas <- rbind(vueltasTodas, vueltas)
    stintsTodos <- rbind(stintsTodos, stints)
  }

  # Los residuos se evaluan con las tres practicas juntas, porque la escala de
  # una practica con pocos residuos se toma de las tres practicas del piloto.
  vueltasTodas <- marcarResiduosAtipicos(vueltasTodas)
  for (i in which(!is.na(resumen$vueltas_candidatas_ajuste))) {
    deSesion <- vueltasTodas$session_key %in% resumen$session_key[i]
    estado <- vueltasTodas$estado_residuo[deSesion]
    resumen$vueltas_stint_corto_o_insuficiente[i] <- sum(estado %in% c("stint_corto", "stint_insuficiente"))
    resumen$vueltas_atipicas[i] <- sum(vueltasTodas$residuo_lento[deSesion] |
                                         vueltasTodas$residuo_rapido[deSesion])
    resumen$vueltas_sin_evaluar_residuo[i] <- sum(
      estado %in% c("no_evaluada_pocos_residuos", "edad_desconocida")
    )
    resumen$vueltas_aptas_ajuste[i] <- sum(vueltasTodas$apta_ajuste[deSesion])
  }

  rownames(resumen) <- NULL
  rownames(vueltasTodas) <- NULL
  rownames(stintsTodos) <- NULL
  return(list(resumen = resumen, vueltas = vueltasTodas, stints = stintsTodos))
}

# Buscar la base dentro de la carpeta del proyecto.
rutaBase <- "datosOpenF1.sqlite3"
if(!file.exists(rutaBase)) {
  stop("No se encontro la base. comproba que abriste el proyecto de RStudio.")
}

# Abrir SQLite en modo lectura.
conexion <- dbConnect(
  SQLite(),
  dbname = rutaBase,
  flags = RSQLite::SQLITE_RO
)

# Leer unicamente las dos tablas del catalogo.
grandesPremios <- dbReadTable(conexion, "grandes_premios")
sesiones <- dbReadTable(conexion, "sesiones")

# Los datos ya estan en la memoria, se cierra la conexion.
dbDisconnect(conexion)

cat("Reuniones leidas:", nrow(grandesPremios), "\n")
cat("Sesiones leidas:", nrow(sesiones), "\n")

# Primer filtro: Conservar unicamente las practicas 1, 2 y 3.
nombresPracticas <- c("Practice 1", "Practice 2", "Practice 3")
practicas <- sesiones[
  sesiones$nombre %in% nombresPracticas,
]

cat("Practicas identificadas:", nrow(practicas), "\n")

# Mostrar algunas columnas de las primeras practicas.
print(head(practicas[, c(
  "session_key",
  "meeting_key",
  "nombre",
  "fecha_fin",
  "cancelada"
)]))

clasificarGranPremio <- function(granPremio, sesiones, ahora) {
  # Comprobar el estado de la reunion en el catalogo.
  if (!isTRUE(granPremio$presente_en_fuente == 1)) {
    return("Reunion ausente del cataologo actual.")
  }
  if (is.na(granPremio$cancelado)) {
    return("Cancelacion de la reunion desconocida")
  }
  if (granPremio$cancelado == 1) {
    return("Reunion cancelada")
  }
  
  # Obtener las sesiones vigentes de esta reunion.
  sesionesGP <- sesiones[
    sesiones$meeting_key %in% granPremio$meeting_key & sesiones$presente_en_fuente %in% 1,
  ]
  if (any(grepl("sprint", sesionesGP$nombre, ignore.case = TRUE))) {
    return("Fin de semana sprint")
  }
  
  # Buscar las tres practicas por sus nombres.
  nombresNecesarios <- c("Practice 1", "Practice 2", "Practice 3")
  practicasGP <- sesionesGP[
    sesionesGP$nombre %in% nombresNecesarios,
  ]
  
  if (!all(nombresNecesarios %in% practicasGP$nombre)) {
    return("Faltan practicas en el catalogo")
  }
  if (anyDuplicated(practicasGP$nombre) > 0) {
    return("Registros ambiguos de practicas")
  }
  if (anyNA(practicasGP$cancelada)) {
    return("Cancelacion de alguna practica desconocida")
  }
  if (any(practicasGP$cancelada == 1)) {
    return("Alguna practica cancelada")
  }
  
  # Python normalizo estas fechas del catalogo a UTC.
  inicio <- as.POSIXct(
    practicasGP$fecha_inicio,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
  fin <- as.POSIXct(
    practicasGP$fecha_fin,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
  
  if (anyNA(inicio) || anyNA(fin) || any(fin <= inicio)) {
    return("Fechas de practicas incompletas o invalidas")
  }
  if (any(fin > ahora)) {
    return("Todavia no terminaron las tres practicas")
  }
  
  return("Candidato para revisar clima")
}

# Preparar una tabla de revision con una fila por reunion.
revisionGP <- grandesPremios[, c(
  "meeting_key",
  "temporada",
  "nombre"
)]

revisionGP$estado <- NA_character_

# Usar el mismo instante para todas las evaluaciones.
ahora <- Sys.time()

for(i in seq_len(nrow(grandesPremios))) {
  revisionGP$estado[i] <- clasificarGranPremio(
    grandesPremios[i, ],
    sesiones,
    ahora
  )
}

# Mostrar cuantas reuniones quedaron en cada categoria.
print(table(revisionGP$estado))

gpCandidatos <- revisionGP[
  revisionGP$estado == "Candidato para revisar clima",
]
gpExcluidos <- revisionGP[
  revisionGP$estado != "Candidato para revisar clima",
]

cat("GP candidatos:", nrow(gpCandidatos), "\n")
cat("Reuniones excluidas:", nrow(gpExcluidos), "\n")

# Caso de prueba: Practice 1 de Bahrein 2023.
sesionPrueba <- 7765
conexion <- dbConnect(
  SQLite(),
  dbname = rutaBase,
  flags = RSQLite::SQLITE_RO
)

# Comprobar el estado de la consulta meteorologica realizada por Python.
estadoClima <- dbGetQuery(
  conexion,
  "
  SELECT estado, filas, exito_en, error
  FROM descargas
  WHERE endpoint = 'weather'
    AND json_extract(parametros_json, '$.session_key') = ?
  ",
  params = list(sesionPrueba)
)

# Leer los registros meteorologicos de esta sesion.
climaPrueba <- dbGetQuery(
  conexion,
  "
  SELECT registro, date, rainfall, track_temperature
  FROM clima
  WHERE session_key = ?
  ORDER BY date, registro
  ",
  params = list(sesionPrueba)
)
dbDisconnect(conexion)
print(estadoClima)

if (
  nrow(estadoClima) != 1 ||
  !isTRUE(estadoClima$estado[1] == "ok")
) {
  stop("La descarga del clima debe revisarse antes de analizar esta practica.")
}

cat("Registros meteorologicos:", nrow(climaPrueba), "\n")
print(head(climaPrueba))

# Mostrar tambien los valores desconocidos, si existen.
print(table(climaPrueba$rainfall, useNA = "ifany"))

# Obtener los datos de la sesion que estamos revisando.
sesionElegida <- sesiones[
  sesiones$session_key %in% sesionPrueba,
]

if (nrow(sesionElegida) != 1) {
  stop("No se encontro una unica sesion para el caso de prueba.")
}

# Convertir el horario de la practica
inicioPractica <- as.POSIXct(
  sesionElegida$fecha_inicio,
  format = "%Y-%m-%dT%H:%M:%OS",
  tz = "UTC"
)
finPractica <- as.POSIXct(
  sesionElegida$fecha_fin,
  format = "%Y-%m-%dT%H:%M:%OS",
  tz = "UTC"
)

# Convertir las fechas de las observaciones meteorologicas.
climaPrueba$fechaUTC <- as.POSIXct(
  climaPrueba$date,
  format = "%Y-%m-%dT%H:%M:%OS",
  tz = "UTC"
)

if (
  anyNA(inicioPractica) ||
  anyNA(finPractica) ||
  anyNA(climaPrueba$fechaUTC)
) {
  stop("Hay fechas desconocidas o invalidas que se deben revisar.")
}

# Ordenar las observaciones cronologicamente.
climaPrueba <- climaPrueba[
  order(climaPrueba$fechaUTC),
]

# Conservar las observaciones dentro del horario de la practica.
climaDurantePractica <- climaPrueba[
  climaPrueba$fechaUTC >= inicioPractica &
    climaPrueba$fechaUTC <= finPractica,
]

# Conservar tambien las observaciones mas proximas a los limites.
muestraAnterior <- tail(
  climaPrueba[climaPrueba$fechaUTC < inicioPractica, ],
  1
)
muestraPosterior <- head(
  climaPrueba[climaPrueba$fechaUTC > finPractica, ],
  1
)

cat("Registros dentro de la practica:", nrow(climaDurantePractica), "\n")
print(head(climaDurantePractica))
print(tail(climaDurantePractica))
print(muestraAnterior[, c("date", "rainfall")])
print(muestraPosterior[, c("date", "rainfall")])

lluviaPreparada <- prepararLluvia(climaPrueba)

resultadoLluvia <- evaluarLluvia(
  lluviaPreparada = lluviaPreparada,
  inicio = inicioPractica,
  fin = finPractica,
  maxHuecoSegundos = 120
)

print(resultadoLluvia)

cat("Observaciones originales:", nrow(climaPrueba), "\n")
cat("Instantes distintos:", nrow(lluviaPreparada), "\n")
print(head(lluviaPreparada))

# Comprobar que todas las observaciones quedaron representadas.
print(
  sum(lluviaPreparada$registros_originales) == nrow(climaPrueba)
)

# Seleccionar las practicas de los Grandes Premios que pasaron los
# filtros del catalogo.
practicasCandidatas <- sesiones[
  sesiones$meeting_key %in% gpCandidatos$meeting_key &
    sesiones$nombre %in% nombresPracticas &
    sesiones$presente_en_fuente %in% 1,
]

# Ordenar por reunion y por nombre de practica.
# Para estos nombres, el orden queda Practice 1, Practice 2 y Practice 3.
practicasCandidatas <- practicasCandidatas[order(
  practicasCandidatas$meeting_key,
  practicasCandidatas$nombre
),
]

# Reiniciar los numeros de fila para facilitar la lectura.
rownames(practicasCandidatas) <- NULL

# Mostrar el tama;o de la lista y algunas filas.
cat(
  "Practicas candidatas para revisar clima:",
  nrow(practicasCandidatas),
  "\n"
)

print(head(practicasCandidatas[, c(
  "meeting_key",
  "session_key",
  "nombre",
  "fecha_inicio",
  "fecha_fin"
)]))

# Comprobar que ninguna sesion aparezca repetida.
sinSesionesRepetidas <- (anyDuplicated(practicasCandidatas$session_key) == 0)

cat("No hay sesiones repetidas:", sinSesionesRepetidas, "\n")

# Abrir la base en modo lecutra.
conexion <- dbConnect(
  SQLite(),
  dbname = rutaBase,
  flags = RSQLite::SQLITE_RO
)

# Leer las observaciones meteorologicas guardadas por Python.
# Conservar session_key para identificar a que sesion pertenece cada fila.
climaCandidatas <- dbGetQuery(
  conexion,
  "
  SELECT session_key, registro, date, rainfall, track_temperature
  FROM clima
  ORDER BY session_key, date, registro
  "
)

# Leer el estado de las descargas meteorologicas.
# Extraer session_key del JSON usado para registrar cada consulta.
descargasClima <- dbGetQuery(
  conexion,
  "
  SELECT
    json_extract(parametros_json, '$.session_key') AS session_key,
    estado,
    filas,
    exito_en,
    error
  FROM descargas
  WHERE endpoint = 'weather'
  "
)

# Cerrar la conexion.
 dbDisconnect(conexion)
 
 # Conservar solamente los datos de las practicas candidatas.
 climaCandidatas <- climaCandidatas[
   climaCandidatas$session_key %in% practicasCandidatas$session_key,
 ]
 descargasClima <- descargasClima[
   descargasClima$session_key %in% practicasCandidatas$session_key,
 ]
 
 # Mostrar cuanta informacion meteorologica se encontro.
 cat(
   "Observaciones meteorologicas de las practicas candidatas:",
   nrow(climaCandidatas),
   "\n"
 )
 cat(
   "Practicas candidatas con observaciones meteorologicas:",
   length(unique(climaCandidatas$session_key)),
   "de",
   nrow(practicasCandidatas),
   "\n"
 )
 
 # Mostrar los estados de las descargas que tienen registro.
 print(table(descargasClima$estado, useNA = "ifany"))
 
 # Identificar sesiones que no tienen un registro de descarga.
 sinRegistroDescarga <- practicasCandidatas[
   !practicasCandidatas$session_key %in% descargasClima$session_key,
 ]
 
 cat(
   "Practicas sin registro de descarga meteorologica:",
   nrow(sinRegistroDescarga),
   "\n"
 )
 
 # Cada sesion deberia tener un unico registro de descarga de weather.
 cat(
   "No hay registros de descarga repetidos por sesion:",
   anyDuplicated(descargasClima$session_key) == 0,
   "\n"
 )

# Probar la revision completa con la primera practica candidata.
revisionPrueba <- revisarClimaPractica(
  practica = practicasCandidatas[1, ],
  clima = climaCandidatas,
  descargas = descargasClima,
  maxHuecoSegundos = 120
)

print(revisionPrueba)

# Preparar una lista con un lugar para el resultado de cada practica.
resultadosClima <- vector(mode = "list", length = nrow(practicasCandidatas))

# Recorrer todas las practicas candidatas.
for (i in seq_len(nrow(practicasCandidatas))) {
  # Obtener la fila de la practica actual.
  practicaActual <- practicasCandidatas[i, ]
  
  # Comprobar sus datos y evaluar la lluvia.
  evaluacion <- revisarClimaPractica(
    practica = practicaActual,
    clima = climaCandidatas,
    descargas = descargasClima,
    maxHuecoSegundos = 120
  )
  
  # Agregar los identificadores para saber a que practica corresponde
  # cada resultado.
  resultadosClima[[i]] <- cbind(
    practicaActual[, c(
      "meeting_key",
      "session_key",
      "nombre"
    )],
    evaluacion
  )
}

# Unir los resultados en una tabla con una fila por practica.
revisionClima <- do.call(rbind, resultadosClima)

# Reiniciar los numeros de fila para facilitar la lectura.
rownames(revisionClima) <- NULL

# Mostrar la cantidad de practicas revisadas y sus clasificaciones.
cat("Practicas revisadas:", nrow(revisionClima), "\n")
print(table(revisionClima$estado))

# Mostrar las primeras filas de la tabla.
print(head(revisionClima))

# Comprobar que haya exactamente un resultado por practica candidata.
unaRevisionPorPractica <- (
  nrow(revisionClima) == nrow(practicasCandidatas) &&
    anyDuplicated(revisionClima$session_key) == 0 &&
    setequal(
      revisionClima$session_key,
      practicasCandidatas$session_key
    )
)
cat(
  "Hay una revision por cada practica candidata:",
  unaRevisionPorPractica,
  "\n"
)

# Crear una tabla con una fila por Gran Premio candidato.
resumenClimaGP <- gpCandidatos[, c(
  "meeting_key",
  "temporada",
  "nombre"
)]

# Preparar las columnas donde vamos a guardar los conteos.
resumenClimaGP$practicas_completas <- 0L
resumenClimaGP$practicas_parciales <- 0L
resumenClimaGP$practicas_descartadas <- 0L
resumenClimaGP$practicas_indeterminadas <- 0L

# Recorrer los Grandes Premios candidatos.
for (i in seq_len(nrow(resumenClimaGP))) {
  # Obtener las evaluaciones de las practicas de este GP.
  practicasGP <- revisionClima[
    revisionClima$meeting_key %in% resumenClimaGP$meeting_key[i],
  ]
  
  # Contar las practicas de cada categoria.
  resumenClimaGP$practicas_completas[i] <- sum(
    practicasGP$estado == "candidato_completo"
  )
  
  resumenClimaGP$practicas_parciales[i] <- sum(
    practicasGP$estado == "candidato_parcial"
  )
  
  resumenClimaGP$practicas_descartadas[i] <- sum(
    practicasGP$estado == "descartar_inicio_con_lluvia"
  )
  
  resumenClimaGP$practicas_indeterminadas[i] <- sum(
    practicasGP$estado == "indeterminado"
  )
}

# Sumar las practicas que conservan un tramo candidato.
resumenClimaGP$practicas_con_tramo <- (
  resumenClimaGP$practicas_completas +
    resumenClimaGP$practicas_parciales
)

# Contar todas las categorias para comprobar las tres practicas de cada GP.
totalPracticasGP <- (
  resumenClimaGP$practicas_completas +
    resumenClimaGP$practicas_parciales +
    resumenClimaGP$practicas_descartadas +
    resumenClimaGP$practicas_indeterminadas
)
cat(
  "Cada GP tiene sus tres practicas clasificadas:",
  all(totalPracticasGP == 3),
  "\n"
)

# Separar los GP que conservan al menos un tramo candidato.
gpConTramosCandidatos <- resumenClimaGP[
  resumenClimaGP$practicas_con_tramo > 0,
]

gpSinTramosCandidatos <- resumenClimaGP[
  resumenClimaGP$practicas_con_tramo == 0,
]

# Mostrar resultado general.
cat(
  "GP con al menos un tramo candidato:",
  nrow(gpConTramosCandidatos),
  "\n"
)
cat(
  "GP sin tramos candidatos",
  nrow(gpSinTramosCandidatos),
  "\n"
)

# Pedir la seleccion al ejecutar desde la consola de R o Source de RStudio.
# En una ejecucion automatica con Rscript se omiten los menus para no esperar
# respuestas. Las funciones se pueden comprobar pasando respuestas de prueba.
seleccionUsuario <- NULL
if (interactive()) {
  seleccionUsuario <- seleccionarAnalisis(
    gpDisponibles = gpConTramosCandidatos,
    revisionClima = revisionClima,
    rutaBase = rutaBase
  )

  if (is.null(seleccionUsuario)) {
    cat("No se completo la seleccion.\n")
  } else {
    cat("\nSeleccion realizada:\n")
    cat("Temporada:", seleccionUsuario$temporada, "\n")
    cat("Gran Premio:", seleccionUsuario$granPremio$nombre, "\n")
    cat("Piloto:", seleccionUsuario$piloto$full_name,
        "- dorsal", seleccionUsuario$piloto$driver_number, "\n")
    cat("Practicas con tramo candidato:", nrow(seleccionUsuario$tramos), "\n")
    cat("Siguiente paso: comprobar las vueltas y compuestos del piloto elegido.\n")
  }
} else {
  cat("Menus omitidos en ejecucion no interactiva. Usa Source en RStudio para elegir.\n")
}

# Revisar las tres practicas cuando se completo la seleccion.
# Solo las que conservan un tramo meteorologico candidato aportan vueltas.
revisionPiloto <- NULL
if (!is.null(seleccionUsuario)) {
  revisionPiloto <- revisarPracticasPiloto(
    seleccion = seleccionUsuario,
    revisionClima = revisionClima,
    rutaBase = rutaBase
  )

  vueltasPiloto <- revisionPiloto$vueltas
  stintsPiloto <- revisionPiloto$stints
  vueltasEnTramo <- vueltasPiloto[
    vueltasPiloto$estado_horario == "dentro_del_tramo",
  ]
  vueltasConStint <- vueltasEnTramo[
    vueltasEnTramo$estado_stint == "asignado" & !vueltasEnTramo$numero_repetido,
  ]

  cat("\nRevision de las tres practicas para", seleccionUsuario$piloto$full_name, "\n")
  print(revisionPiloto$resumen)
  cat("Vueltas registradas del piloto:", nrow(vueltasPiloto), "\n")
  cat("Vueltas completas dentro de los tramos:", nrow(vueltasEnTramo), "\n")
  cat("Vueltas en tramo con stint unico y numero no repetido:", nrow(vueltasConStint), "\n")

  # Candidatas: sin boxes, bandera roja ni VSC/SC identificables.
  # Aptas: ademas en un stint ajustable y sin residuo atipico.
  # Ninguna de las dos garantiza vueltas limpias: son marcas del procedimiento acordado.
  vueltasCandidatas <- vueltasPiloto[vueltasPiloto$candidata_ajuste, ]
  vueltasAptas <- vueltasPiloto[vueltasPiloto$apta_ajuste, ]
  cat("Vueltas candidatas tras exclusiones identificables:", nrow(vueltasCandidatas), "\n")
  cat("Vueltas aptas para el ajuste preliminar:", nrow(vueltasAptas), "\n")
  if (any(vueltasPiloto$estado_residuo == "no_evaluada_pocos_residuos")) {
    cat("Advertencia: hay vueltas aptas sin evaluar sus residuos por falta de datos.\n")
  }
}
