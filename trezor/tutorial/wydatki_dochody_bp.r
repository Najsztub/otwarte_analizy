# MN 21/10/23

# Pakiety
library(data.table)
library(magrittr)
library(ggplot2)
library(gridExtra)

library(knitr)
library(kableExtra)

# Oprócz tego korzystam httr, jsonlite oraz IRkernel do połaczenia z Jupyter

######################################################################
# Używam PostreSQL
library(RPostgreSQL)


# Parametry połączenia z serwerem PostgreSQL zmiennych środowiskowych
# Można zastąpić str
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
    dbname = db_param$base, host = db_param$host

)

GetQuery <- function(x) dbGetQuery(con, x) %>% setDT()

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

pperc <- function(x) sprintf("%1.1f%%", 100*x)

######################################################################
# Obliczenia

lata <- 2015:2023

wydatki_po_cz <- lapply(lata, function(rok) {
    "
    select 
        rok,
        czesc,
        sum(wykonanie + wydatki_wygasle)/1e9 wyd_mld,
        sum(plan)/1e9 wyd_plan_mld
    from trezor.rb28 r 
    left join (
        select distinct on (id_dysp)
        id_dysp,
        id_dysp_nadrz,
        nazwa
        from trezor.slownik_dysponenci
        where date_part('year', to_date(aktywny_od, 'YYYY-MM-DD')) <= %1$s and 
        	(date_part('year', to_date(aktywny_do, 'YYYY-MM-DD')) >= %1$s or aktywny_do = '')
        order by id_dysp, aktywny_od desc
    ) sld
    on r.id_dysp = sld.id_dysp
    where rok = %1$s and miesiac = 12 and id_dysp_nadrz = 1
    group by rok, czesc
    " %>% sprintf(rok) %>% GetQuery()
}) %>% rbindlist()

wyd <- wydatki_po_cz[, .(wyd = sum(wyd_mld), wyd_plan = sum(wyd_plan_mld)), keyby = rok]

# Dochody

dochody <- lapply(lata, function(rok) {
    "
    select 
        rok,
        sum(dochody_wykonane)/1e9 doch,
        sum(plan)/1e9 doch_plan
    from trezor.rb27 r 
    left join (
        select distinct on (id_dysp)
        id_dysp,
        id_dysp_nadrz,
        nazwa
        from trezor.slownik_dysponenci
        where date_part('year', to_date(aktywny_od, 'YYYY-MM-DD')) <= %1$s and 
        	(date_part('year', to_date(aktywny_do, 'YYYY-MM-DD')) >= %1$s or aktywny_do = '')
        order by id_dysp, aktywny_od desc
    ) sld
    on r.id_dysp = sld.id_dysp
    where rok = %1$s and miesiac = 12 and id_dysp_nadrz = 1
    group by rok
    " %>% sprintf(rok) %>% GetQuery()
}) %>% rbindlist()

# Łącz
doch_wyd <- merge(
    wyd,
    dochody,
    all = TRUE,
    by = "rok"
)

doch_wyd[, deficyt_plan := doch_plan - wyd_plan]
doch_wyd[, deficyt_wyk := doch - wyd]
doch_wyd[, deficyt_roznica := deficyt_wyk - deficyt_plan]

doch_wyd

doch_wyd[, .(rok, plan = -deficyt_plan, wykonanie = -deficyt_wyk)] %>%
    melt(id.vars = "rok") %>%
    ggplot(aes(x=rok, y = value, fill = variable)) + geom_area()
