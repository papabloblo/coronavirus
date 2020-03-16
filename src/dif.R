


# DATOS -------------------------------------------------------------------

daily_reports <- readRDS("data/daily_reports_country.RDS")




date_same_level <- function(country1, 
                            country2, 
                            date_max = last_date()
                            ){
  id_days <- lag_days(country1, country2, date_max)
  d <- daily_reports$date[daily_reports$country == country1][id_days]
  if (length(d) == 0){
    d <- NA
  }
  return(d)
}



d <- lapply(unique(daily_reports$date),
        date_same_level,
        country1 = "Italia",
        country2 = "España"
        )

d <- lubridate::as_date(unlist(d))

dif_dias <- tibble(
  country = "España",
  country_ref = "Italia",
  date = unique(daily_reports$date),
  date_same = d
)

dif_dias <- dif_dias %>% 
  mutate(dif = as.numeric(date - date_same)) %>% 
  filter(date >= as.Date("2020-03-01"))


mi <- map_df(1:nrow(dif_dias),
             function(x){
               tibble(
                 date = dif_dias$date[x],
                 bloque = seq(dif_dias$date_same[x],
                              dif_dias$date[x],
                              by = "1 day")
               )
               
             }
)


mi$country <- "España"

mi <- mi %>% 
  left_join(daily_reports)


ggplot(
  data = mi,
  aes(
    x = bloque,
    y = confirmed
  )
) +
  geom_tile(color = "white", height = 500, size = 1) + 
  geom_step(
    data = daily_reports %>% 
      filter(country %in% c("Italia", "España")),
    aes(
      x = date,
      y = confirmed,
      group = country
    )
  )

ggplot(
  data = mi,
  aes(x = bloque,
      y = date)
) +
  geom_tile(color = "white", size = 1) +
  geom_text(
    data = dif_dias,
    aes(
      y = date,
      x = date_same,
      label = dif),
    color = "white"
  ) +
  scale_y_date(date_breaks = "1 day", date_labels = "%d %b")

mi <- mi %>% 
  mutate(dif_dias = as.numeric(date - bloque))

p <- dif_dias %>% 
  filter(date >= as.Date("2020-03-05")) %>% 
  ggplot(
    aes(
      x = date
      )
    ) + 
  geom_rect(
    data = mi %>% 
      filter(date >= as.Date("2020-03-05")),
    aes(xmin = date - 0.5,
        xmax = date + 0.5,
        ymin = dif_dias - 1,
        ymax = dif_dias),
    color = "white",
    fill = "steelblue",
    size = 2
  ) +
  # geom_step(aes(y = dif), color = "orange2", size = 5) +
  geom_text(
    aes(y = dif - 0.5, label = dif, x = date),
    color = "white",
    family = "Oswald",
    size = 12
  ) +
  scale_x_date(date_breaks = "2 days",
               date_labels = "%d %B", ) +
  scale_y_continuous(limits = c(0, NA), breaks = 1:10) +
  # coord_flip() + 
  labs(
    title = "¿Cuántos días hace\nque Italia estaba como España?",
    x = "",
    y = "",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus"
    
  ) + 
  theme(panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank())


ggsave("dataviz/dif_dias.png",
       p, 
       width = 50,
       height = 40, 
       unit = "cm",
       dpi = 320
)


library(tidyverse)
daily_reports <- readRDS("data/daily_reports_country.RDS")
daily_reports2 <- readRDS("data/daily_reports_country.RDS")
daily_reports <- daily_reports %>% 
  filter(country %in% c("Italia", "España"))

dias <- unique(daily_reports$date)
dif_days <- c()
for (i in seq_along(dias)){
  d <- dias[i]
  df <- daily_reports %>% 
    filter(date <= d)
  
  conf <- df$confirmed[df$country == "España" &
                         df$date == d]
  
  first_day <- df$confirmed[df$country == "Italia"] <= conf
  first_day <- which.min(first_day) - 1
  
  if (length(first_day) == 0 || first_day == 0){
    dif_days <- c(dif_days, NA)
  } else {
    dif_days <- c(dif_days, d - df$date[df$country == "Italia"][first_day])
  }
}

dif_dias <- tibble(
  date = dias,
  dif_days = dif_days
)

daily_reports <- daily_reports %>% 
  left_join(dif_dias)


first_day <- daily_reports %>% 
  filter(
    country == "España",
    confirmed >= 100
  )

min(first_day$date)

daily_reports <- daily_reports %>% 
  filter(date >= min(first_day$date))

daily_reports %>% 
  select(date, dif_days)

mi <- Date()
for (i in 1:nrow(daily_reports)){
  mi <- c(mi,
          seq(daily_reports$date[i],
              daily_reports$date[i] + daily_reports$dif_days[i],
              by = "1 day")
          )
           
}

daily_reports <- daily_reports %>% 
  filter(country == "Italia")

mi <- map_df(1:nrow(daily_reports),
    function(x){
      tibble(
        date = daily_reports$date[x],
        bloque = seq(daily_reports$date[x],
                     daily_reports$date[x] + daily_reports$dif_days[x],
                     by = "1 day")
      )
      
    }
    )

mi$dife <- mi$bloque - mi$date
daily_reports %>% 
  left_join(mi) %>%
  filter(dife > 0) %>% 
  ggplot(
    aes(
      x = bloque,
      y = confirmed,
      fill = 1
    )
  ) +
  geom_tile(color = "white", size = 2) +
  geom_text(aes(label = dife), color = "white") + 
  geom_line(data = daily_reports2 %>% 
              filter(country %in% c("Italia", "España"),
                     date >= as.Date("2020-02-15")),
            aes(x = date, y = confirmed, group = country))

