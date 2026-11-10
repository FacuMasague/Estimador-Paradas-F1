library(DBI)
library(RSQLite)

# Formulas del modelo y de las estrategias, separadas de la lectura de datos y menus.
source("modeloEstrategias.R")

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
# filtrarPilotos: funcion opcional (gp, pilotos, tramos) que devuelve solo los pilotos
# elegibles segun el modo; puede agregar la columna etiqueta_extra para el menu.
# Si un GP no tiene pilotos elegibles, se vuelve a pedir el GP.
seleccionarAnalisis <- function(gpDisponibles, revisionClima, rutaBase,
                               leer = readline, filtrarPilotos = NULL) {
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
  repeat {
    opcion <- elegirOpcion("Selecciona el Gran Premio", etiquetasGP, leer)
    if (opcion == 0) return(NULL)
    gp <- opcionesGP[opcion, , drop = FALSE]

    tramos <- revisionClima[
      revisionClima$meeting_key %in% gp$meeting_key &
        revisionClima$estado %in% c("candidato_completo", "candidato_parcial"),
    ]

    pilotos <- leerPilotosPracticas(rutaBase, tramos$session_key)
    if (!is.null(filtrarPilotos) && nrow(pilotos) > 0) {
      cat("Comprobando las vueltas y compuestos de los pilotos de este GP...\n")
      pilotos <- filtrarPilotos(gp, pilotos, tramos)
    }
    if (nrow(pilotos) > 0) break
    cat("No hay pilotos elegibles en este Gran Premio. Elegi otro.\n")
  }

  if (is.null(filtrarPilotos)) {
    cat("\nLa lista de pilotos es preliminar: falta comprobar sus vueltas y compuestos.\n")
  }
  etiquetasPilotos <- paste0(pilotos$full_name, " (dorsal ", pilotos$driver_number, ")")
  if ("etiqueta_extra" %in% names(pilotos)) {
    etiquetasPilotos <- paste(etiquetasPilotos, pilotos$etiqueta_extra)
  }
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
  vueltas$lap_end_stint <- rep(NA_integer_, nrow(vueltas))
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
      vueltas$lap_end_stint[i] <- tanda$lap_end
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
# umbralVueltaLenta: una vuelta mas lenta que umbralVueltaLenta veces la vuelta mas
# rapida del mismo piloto en la sesion (sin contar boxes) se marca vuelta_lenta.
# Saca las vueltas de enfriamiento y las abortadas, que en practica pueden ser la
# mitad de una tanda y rompen el ajuste robusto. 1.10 es un supuesto configurable:
# una tanda larga con mucho combustible suele quedar dentro del 5-8 %.
marcarVueltasNoRepresentativas <- function(vueltas, intervalos, controlVerificado,
                                           umbralVueltaLenta = 1.10) {
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

  # Referencia por sesion y piloto: su vuelta valida mas rapida fuera de boxes.
  vueltas$vuelta_lenta <- rep(FALSE, n)
  validas <- !vueltas$duracion_invalida & !vueltas$salida_boxes &
    !vueltas$entrada_boxes_inferida
  clave <- paste(vueltas$session_key, vueltas$driver_number)
  for (k in unique(clave[validas])) {
    referencia <- min(vueltas$lap_duration[clave == k & validas])
    vueltas$vuelta_lenta[clave == k & validas] <-
      vueltas$lap_duration[clave == k & validas] > umbralVueltaLenta * referencia
  }

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
               "duracion_invalida", "vuelta_lenta", "bandera_roja", "vsc_sc",
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

# Restar el efecto del combustible estimado a cada vuelta de practica.
# La carga real de practica no se publica. Se estima la carga MINIMA que necesitaba
# el auto: combustible para las vueltas que le faltaban al stint (contando la vuelta
# de salida, la de entrada y la actual) mas un margen de margenVueltas vueltas.
#   carga = consumoVuelta * (vueltas del stint - vueltas desde la salida) + consumo * margen
#   tiempo_corregido = lap_duration - sensibilidad * carga
# Asi tiempo_corregido es el tiempo con tanque casi vacio, y la carrera le vuelve a
# sumar su propia carga (predecirVueltas). Supuesto declarado: los simulacros de
# carrera que salen mas pesados de lo que usan quedan con una base algo lenta.
# Simulacro de clasificacion: stint de SOFT con a lo sumo maxVueltasQualy vueltas;
# sus vueltas consumen factorQualy veces el consumo de carrera.
# combustible: lista de combustibleGP() con sensibilidad (s/kg), consumo (kg/vuelta
# de carrera), factorQualy, maxVueltasQualy y margenVueltas.
# Dentro de un stint la correccion suma sensibilidad * consumo a la pendiente y no
# cambia los residuos ni las vueltas aptas.
corregirCombustible <- function(vueltas, combustible) {
  numeros <- unlist(combustible[c("sensibilidad", "consumo", "factorQualy",
                                  "maxVueltasQualy", "margenVueltas")])
  if (length(numeros) != 5 || any(!is.finite(numeros)) || any(numeros < 0)) {
    stop("combustible debe tener sensibilidad, consumo, factorQualy, maxVueltasQualy ",
         "y margenVueltas, todos numeros mayores o iguales a cero.")
  }
  vueltasStint <- vueltas$lap_end_stint - vueltas$lap_start_stint + 1
  vueltas$vueltas_desde_salida <- vueltas$lap_number - vueltas$lap_start_stint
  vueltas$simulacro_qualy <- vueltas$compound %in% "SOFT" &
    vueltasStint <= combustible$maxVueltasQualy
  consumo <- combustible$consumo * ifelse(vueltas$simulacro_qualy, combustible$factorQualy, 1)
  vueltas$carga_estimada <- consumo * (vueltasStint - vueltas$vueltas_desde_salida) +
    combustible$consumo * combustible$margenVueltas
  vueltas$tiempo_corregido <- vueltas$lap_duration -
    combustible$sensibilidad * vueltas$carga_estimada
  return(vueltas)
}

# Parametros de combustible de un GP. El consumo por vuelta de carrera es la carga
# de carrera de su reglamento dividida por las vueltas de la carrera:
# - hasta 2025: kgCarrera["hasta2025"] (105 kg; Heilmeier et al. usan 100-110 kg y el
#   limite de 110 kg no se encontro en los reglamentos revisados);
# - desde 2026: kgCarrera["desde2026"] (70 kg; objetivo de consumo informado por F1,
#   no un limite reglamentario; el reglamento limita la energia, 3000 MJ/h).
# sensibilidad: s/kg = efectoCarrera / kg. efectoCarrera son los segundos que gana el
# auto entre la primera y la ultima vuelta por combustible y evolucion de la pista:
# 3,3 s, mediana de una regresion por carrera (t = piloto + compuesto + edad + vuelta)
# en los 54 GP secos de tres practicas, casos incluidos (3,1 s sin los casos). Asi el
# efecto por vuelta es 3,3 / vueltas, y no depende de la carga supuesta en kg.
# factorQualy (1,25) sale de estimaciones publicas de foros
# (unos 2-2,8 kg/vuelta en clasificacion contra 1,7-2,3 en carrera): baja confianza.
combustibleGP <- function(gp, rutaBase, parametros) {
  distancia <- vueltasDeCarrera(rutaBase, gp$meeting_key)
  if (!is.finite(distancia$valor)) stop("No se conoce la distancia de la carrera del GP.")
  cfg <- parametros$combustible
  kg <- if (gp$temporada >= 2026) cfg$kgCarrera[["desde2026"]] else cfg$kgCarrera[["hasta2025"]]
  list(sensibilidad = cfg$efectoCarrera / kg, consumo = kg / distancia$valor, kgCarrera = kg,
       vueltasCarrera = distancia$valor, factorQualy = cfg$factorQualy,
       maxVueltasQualy = cfg$maxVueltasQualy, margenVueltas = cfg$margenVueltas)
}

# Revisar las tres practicas del GP seleccionado, sin mezclar sus stints.
# Una practica descartada por clima figura en el resumen pero no aporta vueltas.
# Un problema de descarga en una sesion no impide revisar las otras.
revisarPracticasPiloto <- function(seleccion, revisionClima, rutaBase, combustible) {
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
  vueltasTodas <- corregirCombustible(vueltasTodas, combustible)
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

# ---------------------------------------------------------------------------
# Modelo, estrategias y evaluacion. Las formulas estan en modeloEstrategias.R;
# aca solo se leen datos de SQLite y se arma cada caso.
# ---------------------------------------------------------------------------

# Leer el equipo de cada piloto en las practicas seleccionadas.
# Un equipo vacio se ignora: ese piloto queda sin companero (por ejemplo Hungria 2023).
leerEquiposPracticas <- function(rutaBase, clavesSesiones) {
  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(conexion), add = TRUE)
  marcadores <- paste(rep("?", length(clavesSesiones)), collapse = ",")
  equipos <- dbGetQuery(conexion, paste0(
    "SELECT DISTINCT driver_number, team_name FROM pilotos WHERE session_key IN (",
    marcadores, ")"), params = as.list(clavesSesiones))
  equipos <- equipos[!is.na(equipos$team_name) & nzchar(trimws(equipos$team_name)), ]
  # Un dorsal con dos equipos distintos no se asocia a ninguno.
  ambiguos <- equipos$driver_number[duplicated(equipos$driver_number)]
  equipos[!equipos$driver_number %in% ambiguos, ]
}

# Companeros: otros dorsales con el mismo equipo en ese GP (incluye reservas de FP1,
# que usan el mismo auto).
companerosDe <- function(dorsal, equipos) {
  equipo <- equipos$team_name[equipos$driver_number %in% dorsal]
  if (length(equipo) != 1) return(integer())
  setdiff(equipos$driver_number[equipos$team_name == equipo], dorsal)
}

# Revisar las practicas de TODOS los pilotos de un GP y resumir el campo:
# degradacion porcentual por compuesto y bases de cada piloto (modeloEstrategias.R).
campoGP <- function(gp, dorsales, clavesSesiones, revisionClima, rutaBase, parametros) {
  combustible <- combustibleGP(gp, rutaBase, parametros)
  revisiones <- lapply(dorsales, function(d) revisarPracticasPiloto(
    list(granPremio = gp, piloto = data.frame(driver_number = d)),
    revisionClima, rutaBase, combustible))
  names(revisiones) <- as.character(dorsales)
  aptas <- do.call(rbind, lapply(revisiones, function(r) aptasCompuestos(r$vueltas)))
  degradacion <- degradacionCampo(aptas, parametros$minAptas, parametros$minRango,
                                  parametros$minPilotosCampo)
  list(revisiones = revisiones, aptas = aptas, degradacion = degradacion,
       bases = basesCampo(aptas, degradacion, parametros$minAptasBase),
       equipos = leerEquiposPracticas(rutaBase, clavesSesiones),
       combustible = combustible)
}

# Modelo de un piloto a partir del campo de su GP. Con parametros$calibracion se usa
# el modelo calibrado (vigente); con calibracion = NULL, la cadena de respaldo sin
# calibrar, que se conserva para comparar.
modeloDePiloto <- function(dorsal, campo, parametros) {
  if (!is.null(parametros$calibracion)) {
    return(agregarCaida(modeloPilotoCalibrado(dorsal, campo$bases, campo$degradacion,
                                              parametros$calibracion, parametros$pendienteMinima),
                        parametros$caida))
  }
  modeloPiloto(dorsal, campo$bases, campo$degradacion, companerosDe(dorsal, campo$equipos),
               parametros$minPilotosCampo, parametros$pendienteMinima)
}

# Leer la carrera (no Sprint) de un GP con sus descargas verificadas.
leerCarreraGP <- function(rutaBase, meetingKey) {
  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(conexion), add = TRUE)
  sesion <- dbGetQuery(conexion,
    "SELECT * FROM sesiones WHERE meeting_key = ? AND nombre = 'Race'
     AND presente_en_fuente = 1", params = list(meetingKey))
  if (nrow(sesion) != 1) return(list(sesion = sesion, verificada = FALSE))
  clave <- list(sesion$session_key)
  leer <- function(tabla) dbGetQuery(conexion, paste(
    "SELECT * FROM", tabla, "WHERE session_key = ?"), params = clave)
  carrera <- list(sesion = sesion, vueltas = leer("vueltas"), stints = leer("stints"),
                  control = leer("control_carrera"), clima = leer("clima"))
  descargas <- dbGetQuery(conexion,
    "SELECT endpoint, estado, filas FROM descargas
     WHERE endpoint IN ('laps', 'stints', 'race_control', 'weather')
     AND json_extract(parametros_json, '$.session_key') = ?", params = clave)
  filas <- c(laps = nrow(carrera$vueltas), stints = nrow(carrera$stints),
             race_control = nrow(carrera$control), weather = nrow(carrera$clima))
  carrera$verificada <- nrow(descargas) == 4 && anyDuplicated(descargas$endpoint) == 0 &&
    all(descargas$estado == "ok") && all(descargas$filas == filas[descargas$endpoint])
  carrera
}

# Comprobar si una carrera sirve para evaluar en la version de investigacion:
# seca durante la carrera Y en las observaciones previas a la largada (cubren unos
# 50-60 minutos; se exigen al menos minMinutosPrevios), sin bandera roja y sin SC/VSC.
# Los casos con SC/VSC quedan para cuando el algoritmo incluya neutralizaciones.
estadoCarreraEvaluable <- function(carrera, minMinutosPrevios = 30) {
  if (!isTRUE(carrera$verificada)) return("carrera_no_verificada")
  inicio <- aFechaUTC(carrera$sesion$fecha_inicio)
  fin <- aFechaUTC(carrera$sesion$fecha_fin)
  fechas <- aFechaUTC(carrera$clima$date)
  durante <- carrera$clima$rainfall[fechas >= inicio & fechas <= fin]
  previa <- carrera$clima$rainfall[fechas < inicio]
  if (length(durante) == 0 || !all(durante %in% 0)) return("lluvia_en_carrera")
  minutos <- if (length(previa) > 0) {
    as.numeric(difftime(inicio, min(fechas[fechas < inicio]), units = "mins"))
  } else 0
  if (minutos < minMinutosPrevios) return("sin_clima_previo_suficiente")
  if (!all(previa %in% 0)) return("lluvia_antes_de_largar")
  bandera <- carrera$control$flag
  mensaje <- toupper(ifelse(is.na(carrera$control$message), "", carrera$control$message))
  if (any(bandera %in% "RED") || any(mensaje == "SESSION ABORTED")) return("bandera_roja")
  if (any(grepl("SAFETY CAR DEPLOYED|VSC DEPLOYED", mensaje))) return("sc_vsc")
  "ok"
}

# Ultima vuelta real de una carrera: la mayor con alguna duracion registrada.
# En algunas carreras OpenF1 agrega una vuelta final sin duracion para todos (por
# ejemplo Miami 2023 figura con 58 vueltas y se corrieron 57).
ultimaVueltaReal <- function(vueltas) {
  conDuracion <- vueltas$lap_number[is.finite(vueltas$lap_duration)]
  if (length(conDuracion) == 0) NA_real_ else max(conDuracion)
}

# Estrategia real de un piloto en la carrera y si la termino (aunque fuera doblado):
# su ultima vuelta empezo a menos de 150 s de la bandera a cuadros. Si falta el
# mensaje de bandera a cuadros (Monza 2023), se usa el final de la vuelta del lider:
# el mayor (inicio + duracion) de la ultima vuelta real.
estrategiaRealPiloto <- function(carrera, dorsal) {
  s <- carrera$stints[carrera$stints$driver_number %in% dorsal, ]
  s <- s[order(s$stint_number), c("stint_number", "compound", "lap_start", "lap_end",
                                  "tyre_age_at_start")]
  ultima <- ultimaVueltaReal(carrera$vueltas)
  s$lap_end <- pmin(s$lap_end, ultima)
  todas <- carrera$vueltas[carrera$vueltas$lap_number <= ultima, ]
  v <- todas[todas$driver_number %in% dorsal, ]
  bandera <- aFechaUTC(carrera$control$date[carrera$control$flag %in% "CHEQUERED"])
  if (length(bandera) == 0) {
    finLider <- todas[todas$lap_number == ultima & is.finite(todas$lap_duration), ]
    bandera <- aFechaUTC(finLider$date_start) + finLider$lap_duration
  }
  termino <- length(bandera) > 0 && nrow(v) > 0 &&
    isTRUE(as.numeric(difftime(min(bandera), max(aFechaUTC(v$date_start), na.rm = TRUE),
                               units = "secs")) <= 150)
  limites <- nrow(s) > 0 && all(is.finite(c(s$lap_start, s$lap_end))) &&
    all(s$lap_start[-1] == s$lap_end[-nrow(s)] + 1)
  list(stints = s, termino = termino, paradas = nrow(s) - 1,
       compuestos_conocidos = nrow(s) > 0 && all(s$compound %in% compuestosSecos),
       limites_consistentes = limites,
       vueltas_parada = if (nrow(s) > 1) s$lap_end[-nrow(s)] else integer())
}

# Vueltas de carrera del piloto anotadas con las mismas marcas que las practicas.
# Limpias para evaluar: no la vuelta 1 (largada), sin boxes ni neutralizacion,
# con stint unico y numero no repetido.
vueltasCarreraPiloto <- function(carrera, dorsal) {
  v <- carrera$vueltas[carrera$vueltas$driver_number %in% dorsal, ]
  s <- carrera$stints[carrera$stints$driver_number %in% dorsal, ]
  inicio <- aFechaUTC(carrera$sesion$fecha_inicio) - 3600
  fin <- aFechaUTC(carrera$sesion$fecha_fin) + 3 * 3600
  v <- anotarVueltasPiloto(v, s, inicio, fin)
  v <- marcarVueltasNoRepresentativas(v, armarIntervalosControl(carrera$control, fin), TRUE)
  v$limpia_carrera <- v$lap_number > 1 & v$candidata_ajuste
  v[order(v$lap_number), ]
}

# Orden de llegada APROXIMADO de cada carrera desde las vueltas: mas vueltas primero y,
# a igualdad, quien empezo antes su ultima vuelta. No incluye penalizaciones.
reconstruirPosiciones <- function(rutaBase) {
  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(conexion), add = TRUE)
  carreras <- dbGetQuery(conexion, "SELECT session_key, fecha_inicio FROM sesiones
    WHERE nombre = 'Race' ORDER BY fecha_inicio")
  filas <- lapply(carreras$session_key, function(k) {
    v <- dbGetQuery(conexion, "SELECT driver_number, max(lap_number) vueltas,
      max(date_start) ultima FROM vueltas WHERE session_key = ? GROUP BY driver_number",
      params = list(k))
    if (nrow(v) == 0) return(NULL)
    v <- v[order(-v$vueltas, v$ultima), ]
    data.frame(fecha = carreras$fecha_inicio[carreras$session_key == k],
               driver_number = v$driver_number, posicion = seq_len(nrow(v)))
  })
  do.call(rbind, filas)
}

# Posicion media en las n carreras ANTERIORES a la fecha: informacion previa a la largada.
posicionMediaPrevia <- function(posiciones, dorsal, fechaCarrera, n = 5) {
  previas <- posiciones[posiciones$driver_number %in% dorsal &
                          aFechaUTC(posiciones$fecha) < aFechaUTC(fechaCarrera), ]
  previas <- tail(previas[order(previas$fecha), ], n)
  list(media = if (nrow(previas) > 0) mean(previas$posicion) else NA_real_,
       carreras = nrow(previas))
}

# Perdida por parada del circuito, estimada con OTRAS ediciones de su carrera.
# Se prefieren las anteriores; si no hay, se usan las posteriores y se avisa.
# Nunca se usa la carrera evaluada.
estimarPerdidaBoxes <- function(rutaBase, meetingKey) {
  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  ediciones <- dbGetQuery(conexion,
    "SELECT g.meeting_key, g.temporada, s.fecha_inicio FROM grandes_premios g
     JOIN sesiones s ON s.meeting_key = g.meeting_key AND s.nombre = 'Race'
     WHERE g.circuito_key = (SELECT circuito_key FROM grandes_premios WHERE meeting_key = ?)
     ORDER BY s.fecha_inicio", params = list(meetingKey))
  dbDisconnect(conexion)
  fechaObjetivo <- ediciones$fecha_inicio[ediciones$meeting_key == meetingKey]
  otras <- ediciones[ediciones$meeting_key != meetingKey, ]
  anteriores <- if (length(fechaObjetivo) == 1) {
    otras[aFechaUTC(otras$fecha_inicio) < aFechaUTC(fechaObjetivo), ]
  } else otras
  origen <- "ediciones_anteriores"
  usar <- anteriores
  if (nrow(usar) == 0) {
    usar <- otras
    origen <- "ediciones_posteriores"
  }

  perdidas <- numeric()
  usadas <- integer()
  for (m in usar$meeting_key) {
    carrera <- leerCarreraGP(rutaBase, m)
    if (!isTRUE(carrera$verificada) || nrow(carrera$vueltas) == 0) next
    v <- carrera$vueltas
    fin <- aFechaUTC(carrera$sesion$fecha_fin) + 3 * 3600
    intervalos <- armarIntervalosControl(carrera$control, fin)
    intervalos <- intervalos[intervalos$tipo %in% c("bandera_roja", "vsc_sc"), ]
    inicioV <- aFechaUTC(v$date_start)
    finV <- inicioV + ifelse(is.finite(v$lap_duration), v$lap_duration, NA_real_)
    v$neutralizada <- FALSE
    for (k in seq_len(nrow(intervalos))) {
      v$neutralizada <- v$neutralizada |
        (inicioV <= intervalos$hasta[k] & finV >= intervalos$desde[k]) %in% TRUE
    }
    # Una salida de boxes con cambio de neumatico empieza un stint nuevo.
    nuevos <- carrera$stints[carrera$stints$stint_number > 1, ]
    v$cambio_neumatico <- paste(v$driver_number, v$lap_number) %in%
      paste(nuevos$driver_number, nuevos$lap_start)
    p <- calcularPerdidasParadas(v)
    if (length(p) > 0) {
      perdidas <- c(perdidas, p)
      usadas <- c(usadas, usar$temporada[usar$meeting_key == m])
    }
  }
  list(valor = if (length(perdidas) > 0) median(perdidas) else NA_real_,
       origen = if (length(perdidas) > 0) origen else "sin_datos",
       temporadas = usadas, paradas = length(perdidas))
}

# Distancia de la carrera en vueltas: la de la carrera del GP si ya esta en la base
# (distancia programada, conocida antes de largar); si no, la ultima edicion del circuito.
vueltasDeCarrera <- function(rutaBase, meetingKey) {
  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  on.exit(dbDisconnect(conexion), add = TRUE)
  # Solo vueltas con duracion: ver ultimaVueltaReal().
  propia <- dbGetQuery(conexion, "SELECT max(v.lap_number) n FROM vueltas v
    JOIN sesiones s ON s.session_key = v.session_key
    WHERE s.meeting_key = ? AND s.nombre = 'Race' AND v.lap_duration IS NOT NULL",
    params = list(meetingKey))$n
  if (is.finite(propia)) return(list(valor = propia, origen = "carrera_del_gp"))
  otra <- dbGetQuery(conexion, "SELECT max(v.lap_number) n FROM vueltas v
    JOIN sesiones s ON s.session_key = v.session_key
    JOIN grandes_premios g ON g.meeting_key = s.meeting_key
    WHERE s.nombre = 'Race' AND v.lap_duration IS NOT NULL AND g.circuito_key =
      (SELECT circuito_key FROM grandes_premios WHERE meeting_key = ?)
    GROUP BY s.session_key ORDER BY s.fecha_inicio DESC LIMIT 1",
    params = list(meetingKey))$n
  if (length(otra) == 1 && is.finite(otra)) {
    return(list(valor = otra, origen = "ultima_edicion_del_circuito"))
  }
  list(valor = NA_real_, origen = "sin_datos")
}

# Mostrar un valor y permitir cambiarlo con Enter para aceptarlo.
confirmarNumero <- function(texto, valor, leer = readline) {
  if (!interactive() && identical(leer, readline)) return(valor)
  repeat {
    respuesta <- trimws(leer(paste0(texto, " [", valor, "]: ")))
    if (respuesta == "" && is.finite(valor)) return(valor)
    numero <- suppressWarnings(as.numeric(sub(",", ".", respuesta)))
    if (is.finite(numero) && numero > 0) return(numero)
    cat("Ingresa un numero positivo.\n")
  }
}

# Pilotos elegibles de un GP segun el modo:
# - "uso": los 3 compuestos secos con modelo suficiente (practicas; la base puede
#   venir de la cadena de respaldo de modeloPiloto()).
# - "investigacion": ademas, carrera evaluable, piloto terminado, 1 o 2 paradas,
#   compuestos de carrera conocidos. Se ordena primero a los pilotos con posicion
#   media previa en el top 10 (en sus 5 carreras anteriores, al menos 3).
pilotosElegiblesGP <- function(gp, pilotos, tramos, modo, revisionClima, rutaBase,
                               parametros, posiciones = NULL) {
  carrera <- NULL
  if (modo == "investigacion") {
    carrera <- leerCarreraGP(rutaBase, gp$meeting_key)
    if (estadoCarreraEvaluable(carrera) != "ok") return(pilotos[0, ])
  }
  campo <- campoGP(gp, pilotos$driver_number, tramos$session_key, revisionClima,
                   rutaBase, parametros)
  pilotos$etiqueta_extra <- ""
  pilotos$prioridad <- NA_real_
  elegible <- logical(nrow(pilotos))
  for (i in seq_len(nrow(pilotos))) {
    d <- pilotos$driver_number[i]
    modelo <- modeloDePiloto(d, campo, parametros)
    elegible[i] <- all(modelo$suficiente)
    if (elegible[i] && modo == "investigacion") {
      real <- estrategiaRealPiloto(carrera, d)
      elegible[i] <- real$termino && real$paradas %in% 1:2 &&
        real$compuestos_conocidos && real$limites_consistentes
      if (elegible[i] && !is.null(posiciones)) {
        previa <- posicionMediaPrevia(posiciones, d, carrera$sesion$fecha_inicio)
        pilotos$prioridad[i] <- previa$media
        destacado <- isTRUE(previa$media <= 10 && previa$carreras >= 3)
        pilotos$etiqueta_extra[i] <- sprintf("- pos. media previa %.1f%s",
                                             previa$media, if (destacado) " [top 10 previo]" else "")
      }
    }
  }
  elegibles <- pilotos[elegible, ]
  elegibles[order(elegibles$prioridad, na.last = TRUE), ]
}

# Analizar un caso completo: modelo, estrategias y, en investigacion, evaluacion
# contra la carrera real con graficos. Devuelve una lista con todos los resultados.
analizarCaso <- function(seleccion, modo, revisionClima, rutaBase, parametros,
                         leer = readline, carpetaGraficos = "resultados") {
  gp <- seleccion$granPremio
  dorsal <- seleccion$piloto$driver_number
  pilotosGP <- leerPilotosPracticas(rutaBase, seleccion$tramos$session_key)
  dorsales <- unique(c(dorsal, pilotosGP$driver_number))
  campo <- campoGP(gp, dorsales, seleccion$tramos$session_key, revisionClima, rutaBase,
                   parametros)
  modelo <- modeloDePiloto(dorsal, campo, parametros)
  combustible <- campo$combustible

  cat("\n=== Modelo por compuesto (t = base + pendiente * edad) ===\n")
  cat(sprintf(paste("Combustible: %.0f kg de carrera / %d vueltas = %.2f kg/vuelta;",
                    "sensibilidad %.3f s/kg (parametros configurados).\n"),
              combustible$kgCarrera, combustible$vueltasCarrera, combustible$consumo,
              combustible$sensibilidad))
  cat("Base: segundos a edad 0 con tanque casi vacio (carga de practica estimada).\n")
  cat("Pendiente: base x degradacion porcentual de todos los pilotos del GP.\n")
  if (!is.null(parametros$calibracion)) {
    cat("Calibrado: degradacion x factor practica->carrera; SOFT y HARD desde el MEDIUM",
        "propio con la diferencia de carrera; base con correccion de nivel de",
        parametros$calibracion$nivelBase, "% (constantes de parametros$calibracion).\n")
  }
  tabla <- modelo
  tabla$base <- round(tabla$base, 3)
  tabla$degradacion_pct <- round(tabla$degradacion_pct, 3)
  tabla$pendiente <- round(tabla$pendiente, 3)
  tabla$pendiente_usada <- round(tabla$pendiente_usada, 3)
  print(tabla[, c("compound", "suficiente", "origen_base", "base", "origen_pendiente",
                  "degradacion_pct", "pendiente", "pendiente_usada", "vueltas_propias",
                  "pilotos_pendiente", "pilotos_base", "edad_max_observada")],
        row.names = FALSE)
  if (any(modelo$origen_base %in% c("campo_pareado", "companero"))) {
    cat("Advertencia: alguna base sale de la diferencia con el MEDIUM de otros pilotos.\n")
  }
  if (any(modelo$origen_base %in% "igual_medio") ||
      any(modelo$origen_pendiente %in% c("campo_medio", "medio_calibrado"))) {
    cat("Advertencia: alguna degradacion o base se deriva del MEDIUM (escenario",
        "condicionado).\n")
  }
  if (any(modelo$pendiente < modelo$pendiente_usada, na.rm = TRUE)) {
    cat("Advertencia: hay pendientes estimadas menores a", parametros$pendienteMinima,
        "; en las estrategias se usa", parametros$pendienteMinima, ".\n")
  }
  if (!all(modelo$suficiente)) {
    cat("No hay datos suficientes para los tres compuestos: no se buscan estrategias.\n")
    return(list(modelo = modelo))
  }

  distancia <- vueltasDeCarrera(rutaBase, gp$meeting_key)
  cat("\nDistancia de carrera:", distancia$valor, "vueltas (", distancia$origen, ")\n")
  nVueltas <- round(confirmarNumero("Vueltas de carrera", distancia$valor, leer))
  combustible$consumo <- combustible$kgCarrera / nVueltas
  boxes <- estimarPerdidaBoxes(rutaBase, gp$meeting_key)
  cat("Perdida por parada estimada:", round(boxes$valor, 2), "s (", boxes$origen,
      "; temporadas", paste(unique(boxes$temporadas), collapse = ", "), ";",
      boxes$paradas, "paradas)\n")
  if (boxes$paradas < 5) {
    cat("Advertencia: la perdida se estimo con menos de 5 paradas; conviene revisarla",
        "e ingresar un valor documentado.\n")
  }
  if (boxes$origen == "ediciones_posteriores") {
    cat("Advertencia: no hay ediciones anteriores; se uso una edicion posterior del circuito.\n")
  }
  perdida <- confirmarNumero("Perdida por parada (s)", round(boxes$valor, 2), leer)
  if (!is.finite(nVueltas) || !is.finite(perdida)) {
    cat("Faltan la distancia o la perdida por parada: no se buscan estrategias.\n")
    return(list(modelo = modelo))
  }

  # Reglamento deportivo 2025, art. 30: en Monaco cada piloto debe usar al menos tres
  # juegos, es decir, dos paradas. 2023, 2024 y 2026 no se verificaron para Monaco.
  paradas <- parametros$paradas
  if (gp$temporada == 2025 && isTRUE(gp$nombre == "Monaco Grand Prix")) {
    paradas <- intersect(paradas, 2)
    cat("Monaco 2025: el reglamento exige tres juegos; solo se comparan estrategias de 2 paradas.\n")
  }
  estrategias <- generarEstrategias(nVueltas, modelo, perdida, paradas,
                                    parametros$minVueltasStint, parametros$edadInicial,
                                    parametros$maxFactorEdad, parametros$vidaMaxima)
  if (nrow(estrategias) == 0) {
    cat("No hay estrategias admisibles con las restricciones configuradas",
        "(revisar maxFactorEdad, vidaMaxima o minVueltasStint).\n")
    return(list(modelo = modelo))
  }
  mejores <- mejoresEstrategias(estrategias, parametros$maxEstrategias)
  recomendada <- list(
    compuestos = strsplit(mejores$secuencia[1], "-")[[1]],
    largos = diff(c(0, as.numeric(strsplit(mejores$vueltas_parada[1], "-")[[1]]), nVueltas)))
  cat("\n=== Mejores estrategias del modelo (juegos nuevos, sin neutralizaciones) ===\n")
  cat("En este modelo el orden de los stints no cambia el tiempo: se muestra un orden",
      "por cada combinacion de compuestos.\n")
  salida <- mejores[, c("paradas", "secuencia", "vueltas_parada", "stints", "diferencia",
                        "extrapola")]
  salida$diferencia <- round(salida$diferencia, 1)
  print(salida, row.names = FALSE)
  # Margen de empate: con la calibracion vigente se simula su incertidumbre.
  real <- NULL
  carrera <- NULL
  if (modo == "investigacion") {
    carrera <- leerCarreraGP(rutaBase, gp$meeting_key)
    real <- estrategiaRealPiloto(carrera, dorsal)
  }
  margen <- list(margen = NA_real_, prob_real_mejor = NA_real_)
  empatadas <- character()
  if (!is.null(parametros$calibracion) && !is.null(parametros$incertidumbre)) {
    inc <- parametros$incertidumbre
    margen <- margenEmpate(modelo, nVueltas, perdida, inc$residuos, recomendada,
                           if (!is.null(real)) list(compuestos = real$stints$compound,
                                                    largos = diff(c(0, real$vueltas_parada, nVueltas))),
                           paradas, parametros$minVueltasStint, parametros$edadInicial,
                           parametros$maxFactorEdad, inc$simulaciones, inc$cuantil,
                           vidaMaxima = parametros$vidaMaxima)
    empatadas <- combinacionesEmpatadas(estrategias, margen$margen)
    cat(sprintf(paste("Margen de empate: %.1f s (la recomendada pierde menos que eso frente a",
                      "la mejor en el %.0f %% de %d simulaciones de la calibracion).\n"),
                margen$margen, 100 * inc$cuantil, inc$simulaciones))
    cat("Combinaciones empatadas con la mejor:", paste(empatadas, collapse = "; "), "\n")
  } else {
    cat("Diferencias de pocas decimas no son ventajas demostradas.\n")
  }
  if (any(mejores$extrapola)) {
    cat("Advertencia: alguna estrategia usa edades mayores a las observadas (extrapolacion).\n")
  }

  neutralizaciones <- NULL
  if (!is.null(parametros$neutralizacion)) {
    fechaCarrera <- if (!is.null(carrera)) carrera$sesion$fecha_inicio else
      sesiones$fecha_inicio[sesiones$meeting_key == gp$meeting_key & sesiones$nombre == "Race"][1]
    if (is.na(fechaCarrera)) fechaCarrera <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    neutralizaciones <- analizarNeutralizaciones(gp, fechaCarrera, mejores[1, ], modelo, nVueltas,
                                                 perdida, parametros, rutaBase)
  }

  resultado <- list(modelo = modelo, estrategias = estrategias, mejores = mejores,
                    nVueltas = nVueltas, perdida = perdida, boxes = boxes,
                    combustible = combustible, degradacionCampo = campo$degradacion,
                    margen = margen, empatadas = empatadas, neutralizaciones = neutralizaciones)
  if (modo != "investigacion") return(resultado)

  # --- Comparacion con la carrera real (solo investigacion) ---
  vueltasCarrera <- vueltasCarreraPiloto(carrera, dorsal)
  prediccion <- predecirVueltas(real$stints, modelo, combustible, nVueltas)
  comparacion <- compararVueltas(prediccion, vueltasCarrera[vueltasCarrera$limpia_carrera, ])
  largos <- diff(c(0, real$vueltas_parada, nVueltas))
  tiempoReal <- tiempoEstrategia(real$stints$compound, largos, modelo, perdida,
                                 parametros$edadInicial)
  puesto <- puestoEstrategia(tiempoReal, estrategias)

  cat("\n=== Comparacion con la carrera real ===\n")
  cat("Estrategia real:", paste(real$stints$compound, collapse = "-"), "parando al final de",
      paste(real$vueltas_parada, collapse = " y "), "\n")
  cat("Recomendada:", mejores$secuencia[1], "parando al final de",
      gsub("-", " y ", mejores$vueltas_parada[1]), "\n")
  cat(sprintf("Estrategia real en el modelo: %.1f s peor que la mejor; puesto %d de %d.\n",
              tiempoReal - mejores$tiempo_modelo[1], puesto, nrow(estrategias)))
  combinacionReal <- paste(real$paradas, "parada(s):",
                           paste(sort(unique(real$stints$compound)), collapse = "+"))
  realEmpatada <- combinacionReal %in% empatadas
  if (is.finite(margen$margen)) {
    cat(sprintf(paste("Dentro del margen de empate: %s. Combinacion real empatada con la mejor: %s.",
                      "La real es mas rapida que la recomendada en el %.0f %% de las simulaciones.\n"),
                if (tiempoReal - mejores$tiempo_modelo[1] <= margen$margen) "si" else "no",
                if (realEmpatada) "si" else "no", 100 * margen$prob_real_mejor))
  }
  cat(sprintf("Vueltas limpias comparadas: %d\n", comparacion$n))
  cat(sprintf("Error absoluto medio: %.2f s (incluye el desplazamiento de nivel)\n",
              comparacion$mae_absoluto))
  cat(sprintf("Desplazamiento de nivel (mediana real - prevista): %+.2f s\n",
              comparacion$desplazamiento))
  cat(sprintf("Error absoluto medio tras restar ese desplazamiento: %.2f s\n",
              comparacion$mae_ajustado))
  cat("Estas medidas evaluan UN caso; no demuestran que el modelo prediga en general.\n")

  aptas <- campo$aptas[campo$aptas$driver_number %in% dorsal, ]
  caso <- list(comparacion = comparacion, vueltasCarrera = vueltasCarrera,
               paradasReales = real$vueltas_parada,
               paradasRecomendadas = as.numeric(strsplit(mejores$vueltas_parada[1], "-")[[1]]),
               mejores = mejores, diferenciaReal = tiempoReal - mejores$tiempo_modelo[1],
               secuenciaReal = paste(real$stints$compound, collapse = "-"),
               vueltasParadaReal = paste(real$vueltas_parada, collapse = "-"),
               practicaAptas = aptas, modelo = modelo, margen = margen$margen,
               sc = if (is.null(neutralizaciones)) NULL else neutralizaciones$decisiones)
  dir.create(carpetaGraficos, showWarnings = FALSE)
  archivo <- file.path(carpetaGraficos, gsub("[^A-Za-z0-9_]+", "_", paste(
    seleccion$temporada, gp$nombre, seleccion$piloto$driver_number, sep = "_")))
  archivo <- paste0(archivo, ".png")
  if (is.null(caso$sc)) png(archivo, width = 1800, height = 600, res = 110) else
    png(archivo, width = 1500, height = 1100, res = 110)
  graficarCaso(caso)
  dev.off()
  if (interactive()) graficarCaso(caso)
  cat("Graficos guardados en", archivo, "\n")

  c(resultado, list(real = real, comparacion = comparacion, tiempoReal = tiempoReal,
                    puesto = puesto, realEmpatada = realEmpatada, caso = caso,
                    archivoGraficos = archivo))
}

# Historial de neutralizaciones de todas las carreras de la base (solo OpenF1): una fila
# por carrera (seca, con o sin SC/VSC) y los episodios reconstruidos. Se calcula una
# vez por sesion de R y se guarda en memoria; cada caso usa solo carreras ANTERIORES.
.memoNeutralizaciones <- new.env()
historialNeutralizaciones <- function(rutaBase) {
  if (!is.null(.memoNeutralizaciones$h)) return(.memoNeutralizaciones$h)
  conexion <- dbConnect(SQLite(), dbname = rutaBase, flags = RSQLite::SQLITE_RO)
  carreras <- dbGetQuery(conexion, "SELECT s.meeting_key, s.fecha_inicio, g.circuito_key
    FROM sesiones s JOIN grandes_premios g ON g.meeting_key = s.meeting_key
    WHERE s.nombre = 'Race' AND s.presente_en_fuente = 1")
  dbDisconnect(conexion)
  filas <- list(); episodios <- list()
  for (i in seq_len(nrow(carreras))) {
    car <- leerCarreraGP(rutaBase, carreras$meeting_key[i])
    if (!isTRUE(car$verificada) || nrow(car$vueltas) == 0) next
    f <- aFechaUTC(car$clima$date)
    ll <- car$clima$rainfall[f >= aFechaUTC(car$sesion$fecha_inicio) & f <= aFechaUTC(car$sesion$fecha_fin)]
    if (length(ll) == 0 || !all(ll %in% 0) || any(car$stints$compound %in% c("INTERMEDIATE", "WET"))) next
    e <- episodiosNeutralizacion(car$control)
    N <- ultimaVueltaReal(car$vueltas)
    filas[[length(filas) + 1]] <- data.frame(meeting_key = carreras$meeting_key[i],
      fecha = carreras$fecha_inicio[i], circuito_key = carreras$circuito_key[i],
      sc = any(e$tipo == "SC"), vsc = any(e$tipo == "VSC"))
    if (nrow(e)) episodios[[length(episodios) + 1]] <- data.frame(meeting_key = carreras$meeting_key[i],
      fecha = carreras$fecha_inicio[i], tipo = e$tipo, vuelta_inicio = e$vuelta_inicio,
      vueltas = e$vueltas, vueltas_carrera = N)
  }
  .memoNeutralizaciones$h <- list(carreras = do.call(rbind, filas), episodios = do.call(rbind, episodios))
  .memoNeutralizaciones$h
}

# Respuestas ante un SC para la estrategia recomendada: probabilidad del circuito, momento
# tipico, decision vuelta por vuelta, ventanas y medida D. Usa solo carreras anteriores al
# GP. El VSC se dejo afuera: ninguna regla supero a "nunca parar" con datos reales (su
# beneficio depende de donde esta el auto cuando empieza, que no se conoce de antemano).
# Devuelve una lista; imprime un resumen.
analizarNeutralizaciones <- function(gp, fechaCarrera, mejor, modelo, nVueltas, perdida, parametros,
                                     rutaBase) {
  cfg <- parametros$neutralizacion
  h <- historialNeutralizaciones(rutaBase)
  previas <- h$carreras[aFechaUTC(h$carreras$fecha) < aFechaUTC(fechaCarrera), ]
  epPrevios <- h$episodios[aFechaUTC(h$episodios$fecha) < aFechaUTC(fechaCarrera), ]
  if (nrow(previas) < 10) {
    cat("\nSafety car: menos de 10 carreras secas anteriores; no se estiman probabilidades.\n")
    return(NULL)
  }
  circuito <- grandesPremios$circuito_key[grandesPremios$meeting_key == gp$meeting_key]
  prob <- probabilidadNeutralizacion(previas, circuito, cfg$k)
  momento <- momentoNeutralizacion(epPrevios)
  plan <- list(compuestos = strsplit(mejor$secuencia, "-")[[1]],
               vueltasParada = as.numeric(strsplit(mejor$vueltas_parada, "-")[[1]]))
  decisiones <- do.call(rbind, lapply(cfg$tipos, function(t) do.call(rbind, lapply(
    seq_len(nVueltas - 1), function(L) decisionNeutralizacion(plan, L, t, nVueltas, modelo, perdida,
      cfg$factorPerdida, cfg$maxParadas, parametros$minVueltasStint, parametros$edadInicial,
      parametros$maxFactorEdad, parametros$vidaMaxima)))))
  ventanas <- ventanasNeutralizacion(decisiones, nVueltas, cfg$secciones)
  D <- medidaD(plan, decisiones, nVueltas, perdida, cfg$factorPerdida, prob$p, momento, cfg$duracion)

  cat("\n=== Safety car ===\n")
  cat(sprintf("Probabilidad de al menos un SC: %.0f %% (%d carreras secas anteriores; %d de este circuito)\n",
              100 * prob$p[["SC"]], prob$carreras, prob$ediciones_circuito))
  if (prob$ediciones_circuito > 0) {
    cat(sprintf("En este circuito hubo SC en %.0f %% de sus ediciones; se acerca a la tasa general",
                100 * prob$frecuencia_circuito[["SC"]]),
        "porque las diferencias entre circuitos no fueron significativas.\n")
  }
  cat("Momento de inicio del SC (todas las carreras, % por tramo de 20 %):",
      paste(round(100 * momento[, "SC"]), collapse = " / "), "\n")
  cat(sprintf("Perdida por parada: verde %.1f s; bajo SC %.1f s.\n", perdida,
              cfg$factorPerdida[["SC"]] * perdida))
  cat("Respuesta ante un SC que empieza en cada seccion (10 % de la carrera):\n")
  vs <- ventanas$secciones
  vs$ahorro_mediano <- round(vs$ahorro_mediano, 1)
  print(vs[, c("desde", "hasta", "accion", "vueltas_con_esa_respuesta", "vueltas", "siguiente",
               "ahorro_mediano")], row.names = FALSE)
  cat(sprintf(paste("Medida D (ahorro esperado de adaptarse frente al plan fijo): %.1f s por carrera;",
                    "si hay SC: %.1f s. Es una diferencia dentro del simulador.\n"),
              D$esperado, D$condicional[["SC"]]))
  list(probabilidades = prob, momento = momento, decisiones = decisiones, ventanas = ventanas, D = D)
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

# Parametros configurables del modelo. Ninguno esta validado: son supuestos declarados.
parametros <- list(
  combustible = list(         # ver corregirCombustible() y combustibleGP()
    kgCarrera = c(hasta2025 = 105, desde2026 = 70),  # kg de combustible por carrera
    efectoCarrera = 3.3,      # s ganados en toda la carrera (combustible y pista)
    factorQualy = 1.25,       # consumo de un simulacro de clasificacion / carrera
    maxVueltasQualy = 6,      # stint de SOFT con a lo sumo estas vueltas = simulacro
    margenVueltas = 1         # combustible extra, en vueltas de consumo
  ),
  minAptas = 8,               # vueltas aptas para que un piloto aporte degradacion
  minRango = 5,               # rango minimo de edades (vueltas) para aportar degradacion
  minPilotosCampo = 3,        # pilotos minimos para la degradacion o la base pareada
  minAptasBase = 3,           # vueltas aptas propias minimas para una base
  # Practica -> carrera, medido el 10/11/2026 con los 54 GP secos 2023-2026 de tres
  # practicas, INCLUIDOS los casos de investigacion (decision de Facundo: el programa se
  # presenta en la facultad; los resultados de esos casos quedan dentro de la muestra).
  # Mediana entre GP. La carrera sale de una regresion que separa combustible y
  # degradacion (resultados/regresionCarrera.R, calibracion2.R y constantesTodos.R).
  # NULL usa el modelo sin calibrar.
  calibracion = list(
    factorDegradacion = c(SOFT = 0.44, MEDIUM = 0.58, HARD = 0.69),  # deg carrera / practica
    difBase = c(SOFT = -0.01, HARD = 0.03),       # % respecto del MEDIUM en carrera
    relacionDegradacion = c(SOFT = 0.93, HARD = 0.89),  # deg carrera / deg MEDIUM
    # Carrera mas rapida que practicas, en % de la base: mediana de los 50 GP de tres
    # practicas con datos suficientes, casos incluidos (IC 95 %: -1,61 a -0,71; sin los
    # casos, 33 GP, -1,01 %). Ver resultados/errorNivel2.R.
    nivelBase = -1.12
  ),
  minVueltasStint = 5,        # largo minimo de cada stint de carrera
  maxFactorEdad = 3,          # edad final de un stint <= 3 x edad maxima observada
  # Vueltas maximas de un juego en carrera (edad al montarlo + stint): percentil 95 de
  # 3.293 stints de 70 carreras secas 2023-2026 (resultados/vidaStints.R). La caida
  # final no se pudo medir; esto refleja lo que hacen los equipos. NULL = sin limite.
  vidaMaxima = c(SOFT = 31, MEDIUM = 36, HARD = 46),
  pendienteMinima = 0,        # pendiente minima usada en las estrategias (s/vuelta)
  paradas = 1:2,              # cantidades de paradas a comparar
  maxEstrategias = 3,         # estrategias distintas a mostrar
  # VSC/SC (resultados/notas_neutralizaciones.md). NULL = no se analizan.
  neutralizacion = list(
    # Perdida por parada bajo VSC/SC como proporcion de la verde. Medida directa en las
    # paradas hechas: VSC 0,57 y SC 0,50 (resultados/perdidaNeutralizada.R), pero solo
    # incluye a quienes pararon en el momento justo. Calibrado con 280 decisiones reales
    # en 34 episodios (resultados/calibrarFactores.R): SC 0,32 (82 % de acuerdo validado
    # por GP); VSC 1,0, porque ninguna proporcion supera a "nunca parar" (81 %): en la
    # practica un VSC de unas 2 vueltas no abarata la parada lo suficiente.
    factorPerdida = c(VSC = 1.0, SC = 0.32),
    # Tipos que se analizan. El VSC quedo afuera de los resultados (decision de Facundo,
    # 10/11/2026): su beneficio depende de donde esta el auto cuando empieza.
    tipos = "SC",
    # Cuanto acercar la frecuencia del circuito a la tasa general (carreras ficticias):
    # elegido con FastF1 2019-2026 (resultados/calibrarProbabilidadesFF1.R).
    k = c(SC = 8, VSC = Inf),
    duracion = c(SC = 4, VSC = 2),  # vueltas, medianas de 113 episodios
    maxParadas = 2,
    secciones = 10
  ),
  # Edad esperada de cada juego al montarlo en carrera (vueltas). Medida el 29/10/2026
  # con 38 carreras secas que no son casos de investigacion: el 39 % de los stints de
  # SOFT arranca con un juego usado (casi siempre 3 vueltas, de clasificacion), el 12 %
  # de MEDIUM y el 10 % de HARD. Como el tiempo del modelo es lineal en la edad
  # inicial, usar la media da el tiempo esperado. 0 = juegos nuevos.
  edadInicial = c(SOFT = 1.4, MEDIUM = 0.4, HARD = 0.4),
  # Margen de empate: residuos de la calibracion dejando fuera cada GP (mismos 37 GP).
  # degradacion en puntos de % por vuelta; difBase en % de la base del MEDIUM.
  incertidumbre = list(
    simulaciones = 200, cuantil = 0.9,
    residuos = list(
      degradacion = list(
        SOFT = c(0.059, -0.041, -1.138, -0.007, -0.043, 0.059, -0.026, 0.066, 0.16, 0.033,
                 0.125, 0.006, 0.005, 0.103, 0.071, -0.08, 0.022, -0.217, 0.038, 0.049, -0.026,
                 0.073, -0.005, 0.019),
        MEDIUM = c(0.149, -0.077, 0.078, 0.025, 0.028, 0.042, -0.037, 0.039, 0.024, -0.057, 0.032,
                   0.006, 0, 0.035, 0.003, 0.08, -0.031, -0.017, 0, -0.071, -0.038, 0.045, 0.212,
                   0.039, 0.047, -0.011, 0.034, -0.006, 0.077, -0.001, 0.032, -0.039, -0.013,
                   0.057, -0.034, 0.218, 0.03, 0.004, 0.013, -0.018, -0.062, 0.004, -0.07, 0.063,
                   0.2),
        HARD = c(-0.067, 0.052, -0.026, -0.086, 0.011, 0.036, -0.014, -0.031, 0.11, 0.008, 0.07,
                 0, 0.072, 0.109, -0.018, 0.013, 0.017)),
      difBase = list(
        SOFT = c(0.75, -0.6, 0.88, 2.28, 0.28, -0.1, -0.04, 0.78, -0.61, -0.35, 1.76, 8.67,
                 0.57, 0.88, -0.4, 1.9, 0.61, 0.62, -0.22, -0.54, -1.67, -0.03, -3.08, 1.26,
                 -0.68, 0.09, -0.26, -0.51, -0.56, 0.82, 0.06, -0.57, 0.2, -0.3, -0.55, 1.16,
                 -0.9, -1.65, 0.33, 0.1, -0.44, -0.07, 0.42, 0.38, -0.08),
        HARD = c(0.8, -0.32, 0.55, -0.14, -0.27, 0.18, -0.08, -0.23, -0.09, 0.99, -0.18, 0.06,
                 0.09, -0.08, 1, 0.06, 0.13, 0.25, -0.24, -0.15, -0.03, -0.39, -0.51, 0.12,
                 0.25, 0.62, 0.59, 0.55, 0.15, -0.03, 0.28, 0.02, -0.67, 0.01, 0.01, -0.18,
                 -0.29, 0.09, 0.02, -0.52, 0.57, -0.12, 0.45, -0.7, -0.01, -2.6, 0.38, -0.21,
                 0.37, -0.3, -0.64, -0.02)))
  )
)

seleccionUsuario <- NULL
modoAnalisis <- NULL
if (interactive()) {
  opcionModo <- elegirOpcion("Selecciona el modo", c(
    "Investigacion: solo casos evaluables contra la carrera real",
    "Uso: casos con practicas suficientes, sin mirar la carrera"
  ))
  if (opcionModo > 0) {
    modoAnalisis <- c("investigacion", "uso")[opcionModo]
    gpDisponibles <- gpConTramosCandidatos
    posicionesPrevias <- NULL
    if (modoAnalisis == "investigacion") {
      cat("Buscando carreras secas sin bandera roja ni SC/VSC...\n")
      estados <- sapply(gpDisponibles$meeting_key, function(m)
        estadoCarreraEvaluable(leerCarreraGP(rutaBase, m)))
      gpDisponibles <- gpDisponibles[estados == "ok", ]
      posicionesPrevias <- reconstruirPosiciones(rutaBase)
    }
    seleccionUsuario <- seleccionarAnalisis(
      gpDisponibles = gpDisponibles,
      revisionClima = revisionClima,
      rutaBase = rutaBase,
      filtrarPilotos = function(gp, pilotos, tramos) pilotosElegiblesGP(
        gp, pilotos, tramos, modoAnalisis, revisionClima, rutaBase, parametros,
        posicionesPrevias)
    )
  }

  if (is.null(seleccionUsuario)) {
    cat("No se completo la seleccion.\n")
  } else {
    cat("\nSeleccion realizada:\n")
    cat("Temporada:", seleccionUsuario$temporada, "\n")
    cat("Gran Premio:", seleccionUsuario$granPremio$nombre, "\n")
    cat("Piloto:", seleccionUsuario$piloto$full_name,
        "- dorsal", seleccionUsuario$piloto$driver_number, "\n")
    cat("Practicas con tramo candidato:", nrow(seleccionUsuario$tramos), "\n")
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
    rutaBase = rutaBase,
    combustible = combustibleGP(seleccionUsuario$granPremio, rutaBase, parametros)
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

  # Modelo, estrategias y, en investigacion, comparacion con la carrera real.
  resultadoCaso <- analizarCaso(seleccionUsuario, modoAnalisis, revisionClima,
                                rutaBase, parametros)
}
