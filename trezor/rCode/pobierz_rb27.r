# MN 04/03/23

library(data.table)
library(magrittr)
library(ggplot2)

library(httr)
library(jsonlite)

library(RPostgreSQL)

source(file.path("trezor", "rCode", "pobierz_trezor.R"))

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

# Funkcja do wrzucania fragmentów do bazy
dodaj_do_rb27 <- function(dane){
    # Zamień na float
    changeCols <- c(
        "plan",
        "naleznosci",
        "potracenia",
        "dochody_wykonane",
        "dochody_przekazane",
        "naleznosci_do_zaplaty",
        "zaleglosci",
	"nadplaty"
    )
    dane <- dane[, (changeCols) := lapply(.SD, as.numeric), .SDcols = changeCols]
    dbWriteTable(
        con, 
        c("trezor", "rb27"), 
        value = dane, 
        row.names = FALSE, 
        append = TRUE
    )
}

con <- dbConnect(
    drv, 
    user = db_param$user, 
    password = db_param$pass, 
    dbname = db_param$base, 
    host = db_param$host
)

# Ładuj poszczególne lata w pętli
dane_rb27 <- lapply(2023, function(x) {
    message("Rok: ", x)
    start <- 1 
    # if (x == 2015) start <- 1658 + 1
    dane_rok <- pobierz_trezor(
        trezor_api$rb27,
        param = list(
            rok = x,
            miesiac = 3,
            limit = 500,
            page = start,
            format = "json"
        ),
	sleep = 0.8,
        callback = dodaj_do_rb27
    ) %>% rbindlist
})


dbDisconnect(con)
