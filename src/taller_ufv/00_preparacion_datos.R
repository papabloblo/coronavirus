#' Preparación de datos para el taller de coronavirus
#' en las clases de DM1 de la UFV

library(tidyverse)

# CARGA -------------------------------------------------------------------

by_country <- readRDS("data/01_tratamiento/by_country.RDS")


# SOLO ESPAÑA -------------------------------------------------------------

spain <- filter(by_country, country == "Spain") %>% 
  select(-confirmed_acum, -deaths_acum)

readr::write_csv(spain, "data/taller_ufv/spain.csv")

# SOLO ITALIA -------------------------------------------------------------

italy <- filter(by_country, country == "Italy") %>% 
  select(-confirmed_acum, -deaths_acum)

readr::write_csv(italy, "data/taller_ufv/italy.csv")

italy <- italy %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(country, week) %>% 
  summarise(confirmed = sum(confirmed)/1000) %>% 
  ungroup()

readr::write_csv(italy, "data/taller_ufv/italy_week.csv")


# PORTUGAL ----------------------------------------------------------------

portugal <- filter(by_country, country == "Portugal") %>% 
  select(-confirmed_acum, -deaths_acum)

readr::write_csv(italy, "data/taller_ufv/portugal.csv")

portugal <- portugal %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(country, week) %>% 
  summarise(confirmed = sum(confirmed)/1000) %>% 
  ungroup()

readr::write_csv(portugal, "data/taller_ufv/portugal_week.csv")
