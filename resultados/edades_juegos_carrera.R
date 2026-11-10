library(DBI); library(RSQLite)
cal <- read.csv(commandArgs(TRUE)[1]); claves <- cal$meeting_key[!cal$evaluable]
con <- dbConnect(SQLite(), "datosOpenF1.sqlite3", flags = SQLITE_RO)
st <- dbGetQuery(con, sprintf("SELECT g.temporada, s.meeting_key, st.driver_number, st.stint_number, st.compound, st.lap_start, st.lap_end, st.tyre_age_at_start FROM stints st JOIN sesiones s ON s.session_key = st.session_key JOIN grandes_premios g ON g.meeting_key = s.meeting_key WHERE s.nombre = 'Race' AND s.meeting_key IN (%s)", paste(claves, collapse=",")))
dbDisconnect(con)
st <- st[st$compound %in% c("SOFT","MEDIUM","HARD") & is.finite(st$tyre_age_at_start) & is.finite(st$stint_number) & is.finite(st$lap_end), ]
st$largo <- st$lap_end - st$lap_start + 1
st <- st[st$largo >= 5, ]
st$tipo <- ifelse(st$stint_number == 1, "1er", "posterior")
f <- function(x) c(n = length(x), usados = mean(x > 0), mediana = median(x), media = mean(x), media_usados = if (any(x > 0)) mean(x[x > 0]) else NA)
cat("Por tipo de stint:\n"); print(round(do.call(rbind, lapply(split(st$tyre_age_at_start, paste(st$compound, st$tipo)), f)), 2))
s <- st[st$compound=="SOFT",]
cat("\nSOFT por temporada:\n"); print(round(do.call(rbind, lapply(split(s$tyre_age_at_start, s$temporada), f)), 2))
cat("\nEdades SOFT:\n"); print(table(s$tyre_age_at_start))
cat("\nLargo de stint por compuesto (mediana):\n"); print(tapply(st$largo, st$compound, median))
cat("Largo SOFT nuevo vs usado:", median(s$largo[s$tyre_age_at_start==0]), median(s$largo[s$tyre_age_at_start>0]), "\n")
