
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


build_daily_report <- function(){
  
  files <- list.files("data/raw", full.names = TRUE)
  
  daily_reports <- purrr::map_df(
    files, 
    read_daily_csv
  )
  
  # Minúsculas
  names(daily_reports) <- tolower(names(daily_reports))
  
  
  names(daily_reports)[names(daily_reports) == "case-fatality_ratio"] <- "case_fatality_ratio"
  
  # Columnas con la misma información pero distinto nombre
  daily_reports <- daily_reports %>%
    mutate(
      country = ifelse(!is.na(`country/region`), `country/region`, country_region),
      province = ifelse(!is.na(`province/state`), `province/state`, province_state)
    ) %>% 
    select(-`country/region`, -country_region, -`province/state`, -province_state)
  
  return(daily_reports)
}