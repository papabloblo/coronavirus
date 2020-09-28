library(tidyverse)

daily_reports <- readRDS("data/01_tratamiento/daily_reports.RDS")

countries <- daily_reports %>% 
  group_by(country, date) %>% 
  summarise(
    confirmed_acum = sum(confirmed_acum),
    deaths = sum(deaths)
    ) %>% 
  arrange(date) %>% 
  mutate(
    confirmed = confirmed_acum - lag(confirmed_acum, default = 0),
    deaths = deaths_acum - lag(deaths_acum, default = 0)
    )


saveRDS(countries, "data/01_tratamiento/by_country.RDS")


countries %>% 
  filter(country %in% c("Spain", "United Kingdom")) %>% 
  ggplot(aes(x = date, y = confirmed, group = country, colour = country)) +
  geom_line()

