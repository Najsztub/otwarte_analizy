# MN 04/03/23

library(data.table)
library(magrittr)
library(ggplot2)

library(httr)
library(jsonlite)

source(file.path("trezor", "rCode", "pobierz_trezor.R"))

dysp <- pobierz_trezor(
    trezor_api$slownik_dysponenci,
    param = list(
        limit = 500,
        page = 1,
        sort = "id_dysp,aktywny_od",
        format = "json"
    )
) %>% rbindlist

######################
# PIW wysków 16126

piw_wyszkow <- pobierz_trezor(
    trezor_api$rb28,
    param = list(
        id_dysp = 16126,
        rok = 2022,
        limit = 500,
        page = 1,
        sort = "rok,miesiac",
        format = "json"
    )
) %>% rbindlist


############################
# Wszytkie wetery w nazwie

wetery <- dysp[grepl("Wetery", nazwa)]


wetery_2022 <- lapply(wetery$id_dysp, function(id){
    message("Pobieram ", id, " ", dysp[id_dysp == id, nazwa])
    insp <- pobierz_trezor(
        trezor_api$rb28, 
        param = list(
            id_dysp = id,
            rok = 2022,
            limit = 500,
            page = 1,
            sort = "rok,miesiac",
            format = "json"
        )
    ) %>% rbindlist

}) %>% rbindlist()

writexl::write_xlsx(
    list(
        id = unique(wetery, by="id_dysp"),
        rb28 = unique(wetery_2022) %>% 
            merge(unique(wetery, by = "id_dysp"), by = "id_dysp", all.x = TRUE)
    ),
    "dane/piwy_2022_rb28.xlsx"
)
