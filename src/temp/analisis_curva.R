library(tidyverse)

daily_reports <- read_csv("data/daily_reports.zip", 
                          col_types = cols(
                            province = col_character(),
                            country = col_character(),
                            confirmed = col_double(),
                            deaths = col_double(),
                            recovered = col_double(),
                            date = col_date(format = ""),
                            latitude = col_double(),
                            longitude = col_double(),
                            fips = col_character(),
                            admin2 = col_character(),
                            province_state = col_character(),
                            country_region = col_character(),
                            last_update = col_datetime(),
                            lat = col_double(),
                            long_ = col_double(),
                            active = col_double(),
                            combined_key = col_character(),
                            incidence_rate = col_double(),
                            `case-fatality_ratio` = col_double()
                          ))

daily_reports %>% 
  filter(country_region == "Spain") %>% 
  ggplot(aes(x = date, y = deaths)) +
  geom_line()



daily_reports %>% 
  mutate(country_region = ifelse(is.na(country_region), country, country_region) ) %>% 
  filter(country_region == "Spain") %>% 
  group_by(country_region, date) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  ggplot(aes(x = date, y = confirmed)) +
  geom_line()

daily_reports %>% 
  mutate(country_region = ifelse(is.na(country_region), country, country_region) ) %>% 
  filter(country_region == "Spain") %>% 
  group_by(country_region, date) %>% 
  summarise(active = sum(active)) %>% 
  ggplot(aes(x = date, y = active)) +
  geom_line()


df <- daily_reports %>% 
  mutate(country_region = ifelse(is.na(country_region), country, country_region) ) %>% 
  filter(country_region %in% c("Spain", "Italy", "France")) %>% 
  mutate(week = lubridate::week(date)) %>% 
#  mutate(week = date) %>% 
  group_by(country_region, province_state, week) %>% 
  summarise(confirmed = max(confirmed)) %>% 
  ungroup() %>% 
  select(-province_state) %>% 
  group_by(country_region, week) %>% 
  arrange(week) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  mutate(confirmed_inc = confirmed - lag(confirmed))

df %>% 
  filter(week < 39, confirmed_inc >= 0, week != 20) %>% 
  ggplot(aes(x = week, y = confirmed_inc, group=country_region, color=country_region)) +
  geom_line()



df2 <- daily_reports %>% 
  mutate(week = lubridate::week(date)) %>% 
  #  mutate(week = date) %>% 
  group_by(country_region, province_state, week) %>% 
  summarise(confirmed = sum(confirmed)) %>% 
  #ungroup() %>% 
  arrange(week) %>% 
  mutate(confirmed_inc = confirmed - lag(confirmed))

df_ccaa <- daily_reports %>% 
  mutate(country_region = ifelse(is.na(country_region), country, country_region) ) %>% 
  filter(country_region %in% c("Spain")) %>% 
  mutate(week = lubridate::week(date)) %>% 
  #  mutate(week = date) %>% 
  group_by(country_region, province_state, week) %>% 
  summarise(confirmed = max(confirmed),
            incidence_rate = max(incidence_rate)) %>% 
  arrange(week) %>% 
  mutate(confirmed_inc = confirmed - lag(confirmed))


df_ccaa2 <- df_ccaa %>% 
  filter(!is.na(province_state), week < 39)

df_ccaa2 %>% 
  ggplot(aes(x=week, y = confirmed_inc)) +
  geom_line(aes(group=province_state)) +
  geom_line(data = filter(df_ccaa2, province_state == "Madrid"), color="steelblue")


df_ccaa2 %>% 
  ggplot(aes(x=week, y = incidence_rate)) +
  geom_line(aes(group=province_state)) +
  geom_line(data = filter(df_ccaa2, province_state == "Madrid"), color="steelblue")

df2 %>% 
  filter(country_region == "Spain", !is.na(province_state), week < 39) %>% 
  ungroup() %>% 
  arrange(-confirmed_inc)

df_ccaa2 %>% 
  filter(week == max(week))

202688/3052*100000
