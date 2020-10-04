library(tidyverse)

daily_reports <- readRDS("data/01_tratamiento/daily_reports.RDS")
population <- readRDS("data/01_tratamiento/population.RDS")

population <- population %>% 
  filter(country != "US")

daily_reports <- daily_reports %>% 
  left_join(population)

country_province <- daily_reports %>% 
  group_by(country, province) %>% 
  arrange(date) %>% 
  mutate(
     confirmed_acum_14d = confirmed_acum - lag(confirmed_acum, n = 14L, default = 0),
     incidence_100k_14d_acum = confirmed_acum_15d/population*100000,
     
     deaths_acum_14d = deaths_acum - lag(deaths_acum, n = 14L, default = 0),
     deaths_100k_14d_acum = deaths_acum/population*100000
    ) %>% 
  ungroup()


saveRDS(country_province, "data/01_tratamiento/country_province.RDS")
