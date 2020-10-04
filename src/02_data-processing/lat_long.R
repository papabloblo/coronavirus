# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)

source("src/02_data-processing/00_utilities.R")

# LECTURA DE .CSV ---------------------------------------------------------

daily_reports <- build_daily_report()

lat_long <- daily_reports %>% 
  select(
    country,
    province,
    latitude, 
    longitude,
    lat,
    long_
  ) %>% 
  mutate(
    latitude = ifelse(!is.na(latitude), latitude, lat),
    longitude = ifelse(!is.na(longitude), longitude, long_)
  ) %>% 
  select(-lat, -long_) %>% 
  filter(!is.na(latitude)) %>% 
  unique() %>% 
  group_by(country, province) %>% 
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude)
  ) %>% 
  ungroup()

saveRDS(lat_long, "data/01_tratamiento/lat_long.RDS")
