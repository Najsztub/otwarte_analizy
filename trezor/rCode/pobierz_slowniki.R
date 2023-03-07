# MN 04/03/23

library(data.table)
library(magrittr)
library(ggplot2)

library(httr)
library(jsonlite)

library(RPostgreSQL)

source(file.path("trezor", "rCode", "pobierz_trezor.R"))


# Słownik dysponentów

slowniki <- grep("^slownik", names(trezor_api), value = TRUE)


dane_slowniki <- lapply(slowniki, function(x) {
    message("Słownik: ", x)
    pobierz_trezor(
        trezor_api[[x]],
        param = list(
            limit = 500,
            page = 1,
            format = "json"
        )
    ) %>% rbindlist
})

names(dane_slowniki) <- slowniki

######################################################################
# Załaduj do bazy
# Parametry połączenia ze zmiennych środowiskowych
db_param <- list(
  user = Sys.getenv("DB_USER"),
  pass = Sys.getenv("DB_PASS"),
  base = Sys.getenv("DB_BASE"),
  host = Sys.getenv("DB_HOSTNAME")
)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(
    drv, 
    user = db_param$user, 
    password = db_param$pass, 
    dbname = db_param$base, 
    host = db_param$host
)
for (baza in slowniki){

    dbWriteTable(
        con, 
        c("trezor", baza), 
        value = dane_slowniki[[baza]], 
        row.names = FALSE, 
        append = TRUE
    )
}

dbDisconnect(con)
