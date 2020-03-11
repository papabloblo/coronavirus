library(tidyverse)

names(daily_reports) <- tolower(names(daily_reports))
daily_reports <- daily_reports %>% 
  rename(
    province = `province/state`, 
    country = `country/region`
  )

lat_long <- daily_reports %>% 
  select(
    province,
    country,
    latitude, 
    longitude
  ) %>% 
  distinct() %>% 
  filter(!is.na(latitude))


lat_long_country <- lat_long %>% 
  group_by(country) %>% 
  summarise(
    latitude = mean(latitude),
    longitude = mean(longitude)
  )


daily_reports <- daily_reports %>% 
  select(-latitude, -longitude)


daily_reports[is.na(daily_reports)] <- 0

daily_reports <- daily_reports %>% 
  mutate(
    country = case_when(
      country == "Spain" ~ "España",
      country == "Italy" ~ "Italia",
      country == "Mainland China" ~ "China",
      TRUE ~ country
    )
  )

daily_reports_country <- daily_reports %>% 
  group_by(date, country) %>% 
  summarise_at(c("confirmed", "deaths", "recovered"), sum) %>% 
  ungroup()


last_date <- daily_reports %>% 
  filter(country %in% c("Italia", "España")) %>% 
  top_n(1, wt = date)