
# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("src/utils.R")

source("src/tema_ggplot2.R")
theme_set(tema())

first_date <- "2020-02-15"

# DATOS -------------------------------------------------------------------

daily_reports <- readRDS("data/daily_reports_country.RDS")
daily_reports <- daily_reports %>% 
  filter(
    country == "China",
    date >= as.Date(first_date)
    ) %>% 
  pivot_longer(cols = confirmed:recovered_inc,
               names_to = "type",
               values_to = "count"
  ) %>% 
  mutate(
    type = factor(type,
                  levels = 
                    c("confirmed",
                      "recovered",
                      "deaths",
                      "recovered_porc",
                      "deaths_porc",
                      "confirmed_inc",
                      "recovered_inc",
                      "deaths_inc"
                      )
                  )
    )


last_deaths_porc <- daily_reports[daily_reports$type == "deaths_porc" &
                                    daily_reports$date == last_date(),][["count"]]

last_recovered_porc <- daily_reports[daily_reports$type == "recovered_porc" &
                                       daily_reports$date == last_date(),][["count"]]


# GRÁFICO BASE ------------------------------------------------------------

p <- daily_reports %>% 
  filter(type %in% c("recovered_porc", "deaths_porc")) %>% 
  ggplot(
    aes(x = date,
        y = count,
        group = type,
        fill = type
        )
  )



# ÁREAS -------------------------------------------------------------------

p <- p +
  geom_area()

p <- p +
  annotate("text", 
           x = last_date() - 1, 
           y = .4, 
           label = "Recuperados", 
           color = "white",
           size = 13, 
           family = "Oswald", 
           hjust = "right"
           ) +
  annotate("text",
           x = last_date() - 1,
           y = .02, 
           label = "Muertos", 
           color = "white",
           size = 10, 
           family = "Oswald",
           hjust = "right")


# BARRA -------------------------------------------------------------------

p <- p +
  geom_rect(
    aes(
      xmin = last_date() + 0.25,
      xmax = last_date() + 2.25,
      ymin = 0,
      ymax = last_deaths_porc
    ),
    fill = "#333333"
  ) + 
  annotate(
    "text",
    x = last_date() + 1.25, 
    y = .02, 
    label = scales::percent(last_deaths_porc),
    color = "white",
    size = 8,
    family = "Oswald"
  )



p <- p +
  geom_rect(
    aes(
      xmin = last_date() + 0.25,
      xmax = last_date() + 2.25,
      ymin = last_deaths_porc,
      ymax = last_recovered_porc + last_deaths_porc
    ),
    fill = "steelblue"
  ) +
  annotate(
    "text",
    x = last_date() + 1.25, 
    y = .4, 
    label = scales::percent(last_recovered_porc), 
    color = "white",
    size = 9, 
    family = "Oswald"
  )


p <- p +
  annotate(
    "text",
    x = last_date() + 1.25, 
    y = last_recovered_porc + last_deaths_porc - 0.02, 
    label = "Hoy",
    color = "white",
    size = 13,
    family = "Oswald"
  )

# ESCALA ------------------------------------------------------------------

p <- p +
  scale_fill_manual(values = c("confirmed_porc" = "orange2", 
                               "deaths_porc" = "#333333",
                               "recovered_porc" = "steelblue")
  ) +
  scale_y_continuous(labels = scales::percent, 
                     position = "right")


# ETIQUETAS ---------------------------------------------------------------

p <- p +
  labs(
    title = "Porcentaje de casos recuperados y muertos\nrespecto del total de confirmados en China",
    x = "",
    y = "",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus"
  )

# TEMA --------------------------------------------------------------------

p <- p +
  theme(legend.position = "none")
  
  
 

# EXPORTACIÓN .PNG --------------------------------------------------------

ggsave("dataviz/china.png", 
       p,
       width = 50, 
       height = 40, 
       unit = "cm",
       dpi = 320
       )


