# MN: 04/03/23


# Linki do poszczególnych zbiorów
trezor_api <- list(
    slownik_dysponenci = "https://trezor-api.mf.gov.pl/api/v1/slownik/dysponenci",
    slownik_czesci = "https://trezor-api.mf.gov.pl/api/v1/slownik/czesci",
    slownik_dzialy = "https://trezor-api.mf.gov.pl/api/v1/slownik/dzialy",
    slownik_paragrafy = "https://trezor-api.mf.gov.pl/api/v1/slownik/paragrafy",
    slownik_rozdzialy = "https://trezor-api.mf.gov.pl/api/v1/slownik/rozdzialy",
    slownik_grupy_ekonomiczne = "https://trezor-api.mf.gov.pl/api/v1/slownik/grupy-ekonomiczne",
    slownik_jednostki_budzetowe = "https://trezor-api.mf.gov.pl/api/v1/slownik/jednostki-budzetowe",
    slownik_zrodla_finansowania = "https://trezor-api.mf.gov.pl/api/v1/slownik/zrodla-finansowania",
    rb28 = "https://trezor-api.mf.gov.pl/api/v1/sprawozdania-RB28",
    rb27 = "https://trezor-api.mf.gov.pl/api/v1/sprawozdania-RB27"
)

# Funkcja do zaczytywania całych zbiorów z TERZOR-a
pobierz_trezor <- function(url, param, sleep = 1, callback = NULL){
    wyniki <- list()
    id <- 1
    dalej <- TRUE
    while (dalej) {
        message("Zapytanie: ", id)
        param$page <- id
        odp <- httr::GET(
            url,
            query = param
        )
        # print(odp)
        if (httr::status_code(odp) != 200) {
            dalej <- FALSE
        } else {
            wynik <- fromJSON(httr::content(odp, as = "text"))
            wyniki[[id]] <- data.table(wynik$data)
            if (!is.null(callback)) callback(wyniki[[id]])
            if (is.na(wynik$links$`next`[1])) dalej <- FALSE
            id <- id + 1
            Sys.sleep(sleep)
        } 
    }
    wyniki
}
