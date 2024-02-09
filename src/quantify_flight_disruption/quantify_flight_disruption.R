# orderly::orderly_develop_start(use_draft = TRUE)

iata_data <- readRDS("iata_data.rds")

# Some discrepancies in naming between previously downloaded IATA datasets and newer downloads

iata_data <- iata_data %>%
  dplyr::mutate(orig_country = dplyr::case_when(
    orig_country == "Hong Kong (SAR), China" ~ "Hong Kong",
    orig_country == "Chinese Taipei" ~ "Taiwan",
    TRUE ~ orig_country
  )) %>%
  dplyr::mutate(dest_country = dplyr::case_when(
    dest_country == "Hong Kong (SAR), China" ~ "Hong Kong",
    dest_country == "Chinese Taipei" ~ "Taiwan",
    TRUE ~ dest_country
  ))

# Manually clean names of countries for consistency with population dataset

iata_data <- iata_data %>%
  dplyr::mutate(orig_country = dplyr::case_when(
    orig_country == "Grenada and South Grenadines" ~ "Grenada",
    orig_country == "Guinea Bissau" ~ "Guinea-Bissau",
    orig_country == "Ivory Coast (Cote d'Ivoire)" ~ "Cote d'Ivoire",
    orig_country == "Macedonia" ~ "North Macedonia",
    orig_country == "Swaziland" ~ "Eswatini",
    TRUE ~ orig_country
  )) %>%
  dplyr::mutate(dest_country = dplyr::case_when(
    dest_country == "Grenada and South Grenadines" ~ "Grenada",
    dest_country == "Guinea Bissau" ~ "Guinea-Bissau",
    dest_country == "Ivory Coast (Cote d'Ivoire)" ~ "Cote d'Ivoire",
    dest_country == "Macedonia" ~ "North Macedonia",
    dest_country == "Swaziland" ~ "Eswatini",
    TRUE ~ dest_country
  ))

# What was scale of flight decrease for MERS, Zika and SARS-CoV-2?

# can look at globally and from seed country

outgoing <- iata_data %>%
  filter(orig_country != dest_country) %>%
  group_by(orig_country, year, month) %>%
  summarise(passengers = sum(passengers)) %>%
  mutate(day = "01") %>%
  mutate(date = make_date(year, month, day))

outgoing_all <- iata_data %>%
  filter(orig_country != dest_country) %>%
  group_by(year, month) %>%
  summarise(passengers = sum(passengers)) %>%
  mutate(day = "01")%>%
  mutate(date = make_date(year, month, day))

incoming <- iata_data %>%
  filter(orig_country != dest_country) %>%
  group_by(dest_country, year, month) %>%
  summarise(passengers = sum(passengers)) %>%
  mutate(day = "01")%>%
  mutate(date = make_date(year, month, day))


# Flight change summary function

change_summary <- function(start_month, end_month, flight_df) {

  start_month <- as_date(start_month)
  end_month <- as_date(end_month)

  x <- flight_df %>%
    filter(date > start_month %m-% months(1) & date < end_month %m+% months(1)) %>% # note: %m-% is to subtract dates, %m+% adds months on
    mutate(timing = ifelse(date < start_month %m+% months(12), "year1", "year2")) %>%
    pivot_wider(id_cols = "month",
                names_from = "timing",
                values_from = "passengers")

  monthly_change <- x %>%
    mutate(change = (year2 - year1) / year1)

  yearly_change <- x %>%
    summarise(year1 = sum(year1), year2 = sum(year2)) %>%
    mutate(change = (year2 - year1) / year1,
           month = "total")

  out <- bind_rows(monthly_change, yearly_change)
  out$id <- seq.int(nrow(out))

  out

}

## FIRST LOOK AT GLOBAL FLIGHT VOLUMES

# SARS-CoV-2
sars_period1 <- change_summary("2019-01-01", "2020-12-01", outgoing_all)
sars_period2 <- change_summary("2019-02-01", "2021-01-01", outgoing_all)

sars_period1$pathogen <- "sars"
sars_period2$pathogen <- "sars"
sars_period1$start_timing <- 1
sars_period2$start_timing <- 2

# MERS
mers_period1 <- change_summary("2014-05-01", "2016-04-01", outgoing_all)
mers_period2 <- change_summary("2014-06-01", "2016-05-01", outgoing_all)

mers_period1$pathogen <- "mers"
mers_period2$pathogen <- "mers"
mers_period1$start_timing <- 1
mers_period2$start_timing <- 2

# ZIKA
zika_period1 <- change_summary("2015-02-01", "2017-01-01", outgoing_all)
zika_period2 <- change_summary("2015-03-01", "2017-02-01", outgoing_all)

zika_period1$pathogen <- "zika"
zika_period2$pathogen <- "zika"
zika_period1$start_timing <- 1
zika_period2$start_timing <- 2

# Combine all global flights

global_changes <- bind_rows(sars_period1, sars_period2,
                            mers_period1, mers_period2,
                            zika_period1, zika_period2)

saveRDS(global_changes, "global_flight_changes.rds")

## HERE LOOK AT FLIGHTS FROM SEED COUNTRY

# SARS-CoV-2 and China
origin_china <- outgoing %>%
  filter(orig_country == "China")

sars_china_period1 <- change_summary("2019-01-01", "2020-12-01", origin_china)
sars_china_period2 <- change_summary("2019-02-01", "2021-01-01", origin_china)

sars_china_period1$pathogen <- "sars"
sars_china_period2$pathogen <- "sars"
sars_china_period1$start_timing <- 1
sars_china_period2$start_timing <- 2

# MERS and South Korea
origin_southkorea <- outgoing %>%
  filter(orig_country == "South Korea")

mers_korea_period1 <- change_summary("2014-05-01", "2016-04-01", origin_southkorea)
mers_korea_period2 <- change_summary("2014-06-01", "2016-05-01", origin_southkorea)

mers_korea_period1$pathogen <- "mers"
mers_korea_period2$pathogen <- "mers"
mers_korea_period1$start_timing <- 1
mers_korea_period2$start_timing <- 2

# ZIKA and Brazil
origin_brazil <- outgoing %>%
  filter(orig_country == "Brazil")

zika_brazil_period1 <- change_summary("2015-02-01", "2017-01-01", origin_brazil)
zika_brazil_period2 <- change_summary("2015-03-01", "2017-02-01", origin_brazil)

zika_brazil_period1$pathogen <- "zika"
zika_brazil_period2$pathogen <- "zika"
zika_brazil_period1$start_timing <- 1
zika_brazil_period2$start_timing <- 2


# Combine flights from range of seed locations

seed_location_changes <- bind_rows(sars_china_period1, sars_china_period2,
                                   mers_korea_period1, mers_korea_period2,
                                   zika_brazil_period1, zika_brazil_period2)

saveRDS(seed_location_changes, "seed_location_flight_changes.rds")
