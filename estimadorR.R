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

cat("Observaciones originales:", nrow(climaPrueba), "\n")
cat("Instantes distintos:", nrow(lluviaPreparada), "\n")
print(head(lluviaPreparada))

# Comprobar que todas las observaciones quedaron representadas.
print(
  sum(lluviaPreparada$registros_originales) == nrow(climaPrueba)
)