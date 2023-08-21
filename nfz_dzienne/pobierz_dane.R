# MN: 29/04/23
# Pobieranie dziennych danych z ZUS odnośnie przekazanych składek na NFZ

library(data.table)
library(magrittr)

library(rvest)
library(stringr)
library(lubridate)
library(ggplot2)

# ID wersji
WERSJA <- "210823"

######################################################################
# Ustawienia wyświetlania dla ggplot
theme_set(theme_bw())  
theme_update(legend.position="bottom", text = element_text(size = 20))
# Kolory: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
color_fill <- scale_fill_manual(values = cbp1)
# To use for line and point colors, add
color_color <- scale_colour_manual(values=cbp1)

# Strona ze sładkami NFZ
zus_nfz_glowna <- "https://www.zus.pl/o-zus/o-nas/finanse/przekazywanie-skladek-do-nfz"

zaczytaj_nfz <- function(baza, rok, miesiac) {
    # DEBUG
    # baza <- zus_nfz_glowna
    # rok <- 2003
    # miesiac <- 1
    message(sprintf("Czytam rok %s, msc %s", rok, miesiac))
    strona <- read_html(
        sprintf("%s?year=%s&month=%s", baza, rok, miesiac-1)
    ) %>% 
        html_node(".zus-custom-table")
    if (length(strona) == 0) {
        return(NULL)
    } else {
        dane_nfz <- strona %>% 
            html_table() %>%
            setDT()
        # Usuń puste kolumny
        puste_kol <- which(colnames(dane_nfz) == "")
        if (length(puste_kol) > 0) dane_nfz[, puste_kol] = NULL
        
        colnames(dane_nfz) <- c(
            "data_przekazania", 
            "przekazane", 
            "koszt_poboru", 
            "kumulatywne"
        )
        dane_nfz
    }
}


# Zaczytaj dane po miesiącach
daty <- expand.grid(rok = 2003:2023, miesiac = 1:12)

dane_nfz <- apply(daty, 1, function(data){
    miesiac_nfz <- zaczytaj_nfz(
        zus_nfz_glowna, 
        data['rok'],
        data['miesiac']
    )
    miesiac_nfz
}) %>% rbindlist()

# Zamień daty na format R i liczby ze zł na real

# filtruj i zmień daty
dane_nfz_filtr <- dane_nfz[grepl("[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}", data_przekazania)]
dane_nfz_filtr[, data_przekazania := str_replace_all(data_przekazania, "\\sr\\.", "")]
dane_nfz_filtr[, data_przekazania := dmy(data_przekazania)]

# Zamień liczby
kolumny_zl <- c("przekazane", "koszt_poboru", "kumulatywne")
dane_nfz_filtr[
    , 
    (kolumny_zl) := lapply(
        .SD, 
        function(x) {
            x %>%
            str_replace_all("\\s?zł$", "") %>%
            str_replace_all(",", ".") %>%
            str_replace_all("\\s+", "") %>%
            as.double()
        }
    ),
    .SDcols = kolumny_zl
]

# Wykres NFZ po miesiącach
dane_msc <- dane_nfz_filtr[
    , 
    .(
        nfz_mld = sum(przekazane, na.rm=TRUE) / 1e9
    ), 
    .(data = zoo::as.yearmon(data_przekazania))
][order(data)]
dane_msc %>% 
    ggplot(aes(x = data, y = nfz_mld)) + geom_line() +
    labs(x="Data", y="NFZ mld zł/m-c") +
    theme_bw()

# Podgląd
# plotly::plot_ly(
#     dane_msc, 
#     x = ~data, 
#     y = ~nfz_mld, 
#     type = 'scatter', 
#     mode = 'lines'
# )

# Roczne
dane_nfz_filtr[
    , 
    .(
        nfz_mld = sum(przekazane, na.rm=TRUE) / 1e9
    ), 
    .(data = year(data_przekazania))
][order(data)]

# Cumsum po latach
dane_nfz_filtr[year(data_przekazania) >= 2015][order(data_przekazania)][
    , 
    .(
        data = lubridate::yday(data_przekazania),
        nfz_mld = cumsum(przekazane) / 1e9
    ), 
    .(rok = year(data_przekazania))
] %>% 
    ggplot(aes(x = data, y = nfz_mld, colour=factor(rok))) + geom_line() +
    labs(x="Data", y="NFZ mld zł/m-c")
