nombre_paises <- function(x){
  x %>% 
    mutate(
      country = case_when(
        country == "Spain" ~ "España",
        country == "Italy" ~ "Italia",
        country == "Mainland China" ~ "China",
        country == "France" ~ "Francia",
        country == "South Korea" ~ "Corea del Sur",
        country == "Iran" ~ "Irán",
        country == "Germany" ~ "Alemania",
        country == "Korea, South" ~ "Corea del Sur",
        country == "Iran (Islamic Republic of)" ~ "Irán",
        TRUE ~ country
      )
    )
}

