#' Preparación de datos para el taller de coronavirus
#' en las clases de DM1 de la UFV

library(tidyverse)

# CARGA -------------------------------------------------------------------

daily_reports <- readRDS("data/01_tratamiento/daily_reports.RDS")

countries <- daily_reports %>% 
  group_by(country, date) %>% 
  summarise(
    confirmed_acum = sum(confirmed_acum),
    deaths_acum = sum(deaths)
  ) %>% 
  arrange(date) %>% 
  mutate(
    confirmed = confirmed_acum - lag(confirmed_acum, default = 0),
    deaths = deaths_acum - lag(deaths_acum, default = 0)
  ) %>% 
  ungroup()


# SOLO ESPAÑA -------------------------------------------------------------

spain <- filter(countries, country == "Spain") %>% 
  select(-confirmed_acum, -deaths_acum)

readr::write_csv(spain, "data/taller_ufv/spain.csv")

# SOLO ITALIA -------------------------------------------------------------

italy <- filter(countries, country == "Italy") %>% 
  select(-confirmed_acum, -deaths_acum)

readr::write_csv(italy, "data/taller_ufv/italy.csv")

italy <- italy %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(country, week) %>% 
  summarise(confirmed = sum(confirmed)/1000) %>% 
  ungroup()

readr::write_csv(italy, "data/taller_ufv/italy_week.csv")


# PORTUGAL ----------------------------------------------------------------

portugal <- filter(countries, country == "Portugal") %>% 
  select(-confirmed_acum, -deaths_acum)

readr::write_csv(italy, "data/taller_ufv/portugal.csv")

portugal <- portugal %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(country, week) %>% 
  summarise(confirmed = sum(confirmed)/1000) %>% 
  ungroup()

readr::write_csv(portugal, "data/taller_ufv/portugal_week.csv")
