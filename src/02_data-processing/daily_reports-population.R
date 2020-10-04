
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

source("src/02_data-processing/00_utilities.R")

population <- readRDS("data/01_tratamiento/population.RDS")

# LECTURA DE .CSV ---------------------------------------------------------

daily_reports <- build_daily_report()


# NOMBRE DE VARIABLES ADECUADO --------------------------------------------

# Solo variables relevantes
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


# PROVINCIAS EN CASTELLANO ------------------------------------------------

daily_reports <- daily_reports %>% 
  mutate(province = 
           case_when(
            province == "Andalusia" ~ "Andalucía",
            province == "Castilla y Leon" ~ "Castilla y León",
            province == "Aragon" ~ "Aragón",
            province == "Pais Vasco" ~ "País Vasco",
            province == "Catalonia" ~ "Cataluña",
            TRUE ~ province
            )
         )


# IMPUTACIÓN DE NA --------------------------------------------------------

# Valores no informados imputados a 0
daily_reports <- daily_reports %>% 
  mutate_at(
    vars(confirmed:case_fatality_ratio),
    function(x) ifelse(is.na(x), 0, x)
  )


# DATOS SIN ACUMULAR ------------------------------------------------------

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


# POPULATION --------------------------------------------------------------

population <- daily_reports %>% 
  mutate(
    population = (confirmed_acum/incidence_rate)*100000
  ) %>% 
  select(country, province, population) %>% 
  filter(is.finite(population)) %>% 
  group_by(country, province) %>% 
  summarise(population = mean(population)) %>% 
  ungroup()


# INCIDENCIA ACUMULADA 14 DÍAS --------------------------------------------

daily_reports <- daily_reports %>% 
  left_join(population, by = c("country", "province"))

daily_reports <- daily_reports %>% 
  group_by(country, province) %>% 
  arrange(date) %>% 
  mutate(
    confirmed_acum_14d = confirmed_acum - lag(confirmed_acum, n = 14L, default = 0),
    incidence_100k_14d_acum = confirmed_acum_14d/population*100000,
    
    deaths_acum_14d = deaths_acum - lag(deaths_acum, n = 14L, default = 0),
    deaths_100k_14d_acum = deaths_acum_14d/population*100000
  ) %>% 
  ungroup()

# GUARDADO ----------------------------------------------------------------

saveRDS(daily_reports, "data/01_tratamiento/daily_reports.RDS")
saveRDS(population, "data/01_tratamiento/population.RDS")

