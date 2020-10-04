#'
#' COMPARATIVA DE CASOS CONFIRMADOS ENTRE ITALIA Y ESPAÑA
#' 
#' Salida dataviz/italia_vs_espanya.png
#' 


# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("src/utils.R")

source("src/tema_ggplot2.R")
theme_set(tema())

first_date <- "2020-02-15"

# DATOS -------------------------------------------------------------------

daily_reports <- readRDS("data/daily_reports_country.RDS")
daily_reports <- daily_reports %>% 
  filter(country %in% c("Italia", "España"),
         date >= as.Date(first_date))

date_italy_eq_spain <- date_same_level("Italia", "España")
dif_dias_italia_esp <- last_date() - date_italy_eq_spain

max_scale_y <- 1250*floor(last_confirmed("Italia")/1250)

# GRÁFICO BASE ------------------------------------------------------------

p <- daily_reports %>% 
  ggplot(
    aes(x = date,
        y = confirmed,
        group = country,
        color = country)
  )


# DIFERENCIA DE DÍAS ------------------------------------------------------

p <- p +
  geom_segment(
    x = date_italy_eq_spain + 0.75, 
    xend = last_date() - 0.25,
    y = last_confirmed("España"), 
    yend = last_confirmed("España"),
    color = "#333333",
    alpha = 0.6, 
    size = 1,
    arrow = arrow(ends = "both", type = "closed")
  ) +
  annotate(
    "text",
    x = last_date() - 0.25 - (last_date() - 0.25 - date_italy_eq_spain - 0.75)/2,
    y = last_confirmed("España") + 500, 
    label = paste(dif_dias_italia_esp, "días"),
    size = 8,
    hjust = "center",
    color = "#333333",
    family = "Oswald"
  )


# CURVAS ------------------------------------------------------------------

p <- p +
  geom_line(size = 1.5) +
  geom_text(data = 
              daily_reports %>%
              filter(
                date == last_date(),
                country %in% c("España", "Italia")
                ),
            aes(
              x = date, 
              y = confirmed, 
              label = country
            ),
            nudge_x = 0.15, 
            nudge_y = 150, 
            
            size = 10, 
            
            hjust = "left",
            family = "Oswald"
  )



# ESCALAS -----------------------------------------------------------------

p <- p +
  scale_color_manual(
    values = c("España" = "orange2", 
               "Italia" = "#333333"
    )
  ) +
  scale_x_date(
    breaks = 
      c(
        seq(as.Date("2020-02-17"),
            last_date(),
            by = "1 week"
            ),
        last_date()
        ), 
    labels = 
      function(x) ifelse(x == last_date(),
                         paste0(as.character(x, format = "%d %B"), 
                                "\n(actualización)"
                                ),
                         as.character(x, format = "%d %B")
                         ),
    expand = expand_scale(add = c(0,3))
  ) +
  scale_y_continuous(
    labels = scales::comma_format(), 
    breaks = seq(0, max_scale_y, by = 2500), 
    minor_breaks = seq(0, max_scale_y, 1250), 
    position = "right"
  )



# ETIQUETAS ---------------------------------------------------------------

p <- p +
  labs(
    title = "Evolución de los casos\nconfirmados de coronavirus",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus",
    y = "",
    x = ""
  )


# TEMA --------------------------------------------------------------------

p <- p +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
  )



# EXPORTACIÓN EN .PNG -----------------------------------------------------

ggsave("dataviz/italia_vs_espana.png",
       p, 
       width = 50,
       height = 40, 
       unit = "cm",
       dpi = 320
       )

