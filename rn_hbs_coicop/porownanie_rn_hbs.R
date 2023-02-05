# MN: 05/02/23
# Porównaj konsumpcję COICOP z HBS i RN
# Wymagania: patrz blioteki

library(data.table)
library(magrittr)
library(ggplot2)
library(eurostat)

# Opcje dla pakietu eurostat
# Utwórz katalog na dane
eurostat_cache <- file.path("data", "es_cache")
if(!file.exists(eurostat_cache)) dir.create(eurostat_cache, recursive = TRUE)
# Czy sprawdzać najświeższe dane
options(eurostat_update = FALSE)

# Zaczytaj słownik
slowniki_nazwy <- c("coicop")
slowniki <- lapply(
  slowniki_nazwy,
  function(slownik) {
    eurostat::get_eurostat_dic(slownik) %>% setDT()
  }
)
names(slowniki) <- slowniki_nazwy

# Zaczytaj konsumpcję GD z HBS
kons_hbs <- eurostat::get_eurostat(
  "hbs_exp_t121",
  cache = TRUE,
  cache_dir = eurostat_cache
) %>% setDT()

# Zaczytaj konsumpcję GD z RN
kons_rn <- eurostat::get_eurostat(
  "nama_10_co3_p3",
  cache = TRUE,
  cache_dir = eurostat_cache
) %>% setDT()


# Łącz dane ze sobą
kons_razem <- merge(
  kons_rn[
    unit == "CP_MNAC",
    .(
      time, geo, coicop,
      rn = values
    )
  ],
  kons_hbs[
    ,
    .(
      time, geo, coicop,
      hbs = values
    )
  ],
  by = c("time", "geo", "coicop"),
  all = TRUE
) %>%
  merge(
    slowniki$coicop,
    by.x = "coicop",
    by.y = "code_name",
    all.x = TRUE,
    all.y = FALSE
  )

# Licz udział CP01 w każdym roku
prop_cp01 <- kons_razem[
  coicop == "CP01",
  .(time, geo, prop_01 = rn / hbs)
]
kons_razem <- merge(
  kons_razem,
  prop_cp01,
  by = c("time", "geo"),
  all.x = TRUE
)

# Licz proporcję względem żywności
kons_razem[, blad_rel_cp01 := hbs / rn * prop_01 - 1]

# Przefiltruj brakujące obserwacje
kons_razem_filtr <- kons_razem[!is.na(hbs) & !is.na(rn)]

# Wykres dla PL dla 12 grup COICOP
kons_razem_filtr[
  year(time) == 2020 & geo == "PL" & stringr::str_length(coicop) == 4,
  .(
    coicop,
    prop = blad_rel_cp01,
    kier = ifelse(blad_rel_cp01 > 0, "green", "red")
  )
] %>% ggplot(aes(x = coicop, y = prop, fill = kier)) +
  geom_bar(stat = "identity") + 
  theme_bw() + theme(legend.position = "none") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "COICOP", y = "Proporcja HBS/RN, %"
)


############################ Twórz indeks dla krajów
kons_razem_filtr <- merge(
  kons_razem_filtr,
  kons_razem[coicop == "TOTAL", .(time, geo, rn_total = rn)],
  by = c("time", "geo"),
  all.x = TRUE,
  all.y = FALSE
)

kons_razem_filtr[, blad := rn / rn_total * (blad_rel_cp01)^2]

idx_bledow <- kons_razem_filtr[
  stringr::str_length(coicop) == 4,
  .(
    idx = sqrt(mean(blad))
  ),
  keyby = .(rok = year(time), geo)
]

# Wykres dla wybranych krajów
idx_bledow[
  geo %in% c("PL", "DE", "CZ", "SK", "DK", "SE", "IT", "RO")
] %>%
  ggplot(aes(x = rok, y = idx, color = geo)) + geom_line() + theme_bw()
