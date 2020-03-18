
#' GENERACIÓN DE LOS CONJUNTOS DE DATOS
#' 
#' Lectura de los .csv diarios data/raw/yyyy-mm-dd.csv
#' Se concatenan en un único data.frame 
#' Se agregan los datos a nivel país.
#' Se traducen los nombres de los países relevantes para el estudio.
#' Se calculan porcentajes e incrementos.
#' Se calcula `date_100`, la primera fecha en la que se
#' superaron los 100 casos.
#' 
#' Se generan los archivos:
#'   - data/daily_reports_country.RDS: datos agregados por países.
#'   - data/lat_long.RDS: latitud y longitud de los países
#' 

# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)


# LECTURA DE .CSV ---------------------------------------------------------

files <- list.files("data/raw", full.names = TRUE)


read_daily_csv <- function(file_csv){
  daily_report <- readr::read_csv(file_csv)
  daily_report$`Last Update` <- NULL
  daily_report$date <- as.Date(stringr::str_extract(file_csv,"\\d*-\\d*-\\d*"))
  return(daily_report)
  
}

daily_reports <- purrr::map_df(
  files, 
  read_daily_csv
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
      country == "Spain"                      ~ "España",
      country == "Italy"                      ~ "Italia",
      country == "Mainland China"             ~ "China",
      country == "France"                     ~ "Francia",
      country == "South Korea"                ~ "Corea del Sur",
      country == "Iran"                       ~ "Irán",
      country == "Germany"                    ~ "Alemania",
      country == "Korea, South"               ~ "Corea del Sur",
      country == "Iran (Islamic Republic of)" ~ "Irán",
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

first_100 <- daily_reports_country %>% 
  group_by(country) %>% 
  filter(confirmed >= 100) %>% 
  summarise(date_100 = min(date))

daily_reports_country <- daily_reports_country %>% 
  left_join(first_100)


# GUARDADO ----------------------------------------------------------------

saveRDS(daily_reports_country, "data/daily_reports_country.RDS")
saveRDS(lat_long, "data/lat_long.RDS")
