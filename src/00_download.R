

# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)


# DESCARGA ----------------------------------------------------------------

url_base <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

date_period <- seq(from = as.Date("2020-01-22"),
                   to = Sys.Date() - 1,
                   by = "day") 


read_daily_reports <- function(d){

      daily_report <- readr::read_csv(paste0(url_base,
                                             as.character(d, format = "%m-%d-%Y"),
                                             ".csv"))
      daily_report$`Last Update` <- NULL
      daily_report$date <- as.Date(d)
      return(daily_report)

  }

daily_reports <- purrr::map_df(
  date_period, 
  read_daily_reports
  )


# TRATAMIENTO -------------------------------------------------------------

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
  filter(!is.na(latitude)) %>% 
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

daily_reports_country$confirmed[daily_reports_country$country == "España" &
                                  daily_reports_country$date == as.Date("2020-03-12")] <- 3142
daily_reports_country$confirmed[daily_reports_country$country == "Italia" &
                                  daily_reports_country$date == as.Date("2020-03-12")] <- 15113

daily_reports_country <- daily_reports_country %>% 
  group_by(country) %>% 
  arrange(date) %>% 
  mutate(
    deaths_porc = deaths/confirmed,
    recovered_porc = recovered/confirmed,
    confirmed_inc = confirmed - lag(confirmed)
  ) %>% 
  mutate_at(
    c("confirmed", 
      "deaths", 
      "recovered"),
    list("inc" = function(x) x - lag(x))
  ) %>% 
  ungroup()



# GUARDADO ----------------------------------------------------------------

saveRDS(daily_reports_country, "data/daily_reports_country.RDS")
saveRDS(lat_long, "data/lat_long.RDS")
