
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
  daily_report <- readr::read_csv(file_csv, 
                                  col_types = cols(
                                    FIPS = col_character(),
                                    Admin2 = col_character(),
                                    Province_State = col_character(),
                                    Country_Region = col_character(),
                                    Last_Update = col_datetime(format = ""),
                                    Lat = col_double(),
                                    Long_ = col_double(),
                                    Confirmed = col_double(),
                                    Deaths = col_double(),
                                    Recovered = col_double(),
                                    Active = col_double(),
                                    Combined_Key = col_character(),
                                    Incidence_Rate = col_double(),
                                    `Case-Fatality_Ratio` = col_double()
                                    )
                                  )
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

names(daily_reports)[names(daily_reports) == "case-fatality_ratio"] <- "case_fatality_ratio"

# Columnas con la misma información pero distinto nombre

daily_reports <- daily_reports %>%
  mutate(
    country = ifelse(!is.na(`country/region`), `country/region`, country_region),
    province = ifelse(!is.na(`province/state`), `province/state`, province_state)
  ) %>% 
  select(-`country/region`, -country_region, -`province/state`, -province_state)
  


# Latitud y longitud ------------------------------------------------------

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
  

daily_reports <- daily_reports %>% 
  select(-latitude, -longitude, -lat, -long_)


# Variables relevantes
daily_reports <- daily_reports %>% 
  select(
    country,
    province,
    date, 
    confirmed,
    deaths,
    recovered,
    active,
    incidence_rate,
    case_fatality_ratio
  )

# Valores no informados imputados a 0
daily_reports <- daily_reports %>% 
  mutate_at(
    vars(confirmed:case_fatality_ratio),
    function(x) ifelse(is.na(x), 0, x)
  )

# Cálculo de datos no acumulados

daily_reports <- daily_reports %>% 
  rename(
    confirmed_acum = confirmed,
    deaths_acum = deaths,
    recovered_acum = recovered,
    active_acum = active
  )


daily_reports <- daily_reports %>% 
  group_by(country, province) %>% 
  arrange(date) %>% 
  mutate(
    confirmed = confirmed_acum - lag(confirmed_acum, default = 0),
    deaths = deaths_acum - lag(deaths_acum, default = 0),
    recovered = recovered_acum - lag(recovered_acum, default = 0),
  ) %>% 
  ungroup()


# Población

population <- daily_reports %>% 
  mutate(
    population = (confirmed_acum/incidence_rate)*100000
  ) %>% 
  select(country, province, population) %>% 
  filter(is.finite(population)) %>% 
  group_by(country, province) %>% 
  summarise(population = mean(population)) %>% 
  ungroup()
  

population$population[population$country == "Spain" & is.na(population$province)] <- 47329981

# GUARDADO ----------------------------------------------------------------

saveRDS(daily_reports, "data/01_tratamiento/daily_reports.RDS")
saveRDS(lat_long, "data/01_tratamiento/lat_long.RDS")
saveRDS(population, "data/01_tratamiento/population.RDS")
