#'
#' EVOLUCIÓN DE LOS CASOS CONFIRMADOS DESDE LOS 100 PRIMEROS CASOS
#' 
#' Salida:
#'   - dataviz/confirmados_paises_log.png

# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("src/utils_tratamiento.R")
source("src/utils.R")
source("src/tema_ggplot2.R")
theme_set(tema())


# DATOS -------------------------------------------------------------------

daily_reports <- readRDS("data/daily_reports_country.RDS")

daily_reports <- nombre_paises(daily_reports)

daily_reports <- daily_reports %>% 
  mutate(
    country = ifelse(country == "Corea del Sur",
                     "Corea\ndel Sur",
                     country)
  )

daily_reports <- daily_reports %>% 
  filter(!(is.na(date_100)),
         date >= date_100, 
         country %in% c("España",
                        "Italia", 
                        "Corea\ndel Sur",
                        "Francia",
                        "Irán",
                        "Alemania"
                        )
         ) 

daily_reports <- daily_reports %>% 
  mutate(
    dias = as.numeric(date - date_100),
    country2 = ifelse(
      country %in% c("España"),
      country,
      "Otros"
      )
    )


# BASE --------------------------------------------------------------------

p_conf <- daily_reports %>% 
  # Líneas de fondo: países distintos a España
  filter(country != "España") %>% 
  ggplot(
    aes(
      x = dias,
      y = confirmed,
      group = country
    )
  )

p_muertos <- daily_reports %>% 
  # Líneas de fondo: países distintos a España
  filter(country != "España") %>% 
  ggplot(
    aes(
      x = dias,
      y = deaths,
      group = country
    )
  )


# LÍNEAS FONDO ------------------------------------------------------------

p_conf <- p_conf +
  geom_line(aes(linetype = country), size = 1, alpha = 0.7)

p_muertos <- p_muertos +
  geom_line(aes(linetype = country), size = 1, alpha = 0.7)


# LÍNEA DE ESPAÑA ---------------------------------------------------------

p_conf <- p_conf +
  geom_line(data = daily_reports %>% filter(country == "España"), 
            aes(color = country), 
            size = 3)

p_muertos <- p_muertos +
  geom_line(data = daily_reports %>% filter(country == "España"), 
            aes(color = country), 
            size = 3)


# CARTELES DE LOS PAÍSES --------------------------------------------------

p_conf <- p_conf +
  geom_label(
    data =
      filter(daily_reports, date == last_date()),
    aes(
      y = confirmed,
      label = country,
      color = country2
      ),
    
    size = 10,
    hjust = "center",
    family = "Oswald",
    label.r = unit(0, "lines"),
  ) 

p_muertos <- p_muertos +
  geom_label(
    data =
      filter(daily_reports, date == last_date()),
    aes(
      y = deaths,
      label = country,
      color = country2
    ),
    
    size = 10,
    hjust = "center",
    family = "Oswald",
    label.r = unit(0, "lines"),
  ) 



# ETIQUETAS ---------------------------------------------------------------

p_conf <- p_conf +
  labs(
    title = "Evolución después de los\n100 primeros casos confirmados",
    x = "",
    y = "",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus"
  )

p_muertos <- p_muertos +
  labs(
    title = "Evolución de las muertes después de los\n100 primeros casos confirmados",
    x = "",
    y = "",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus"
  )



# ESCALAS -----------------------------------------------------------------

p_conf <- p_conf +
  scale_x_continuous(breaks = seq(0, 25, by = 2), 
                     labels = function(x) ifelse(x == 2,
                                                 paste("+", x, "días"),
                                                 x)
                     ) +
  scale_y_log10(position = "right",
                labels = scales::comma_format(),
                breaks = c(100, 500, 1000, 2500, 5000, 20000)) +
  scale_color_manual(
    values = c("España" = "orange2",
               "Otros" = "#333333",
               "China" = "#333333",
               "South Korea" = "#333333"
    )
  ) 

p_muertos <- p_muertos +
  scale_x_continuous(breaks = seq(0, 25, by = 2), 
                     labels = function(x) ifelse(x == 2,
                                                 paste("+", x, "días"),
                                                 x)
  ) +
  scale_y_log10(position = "right",
                labels = scales::comma_format()
                ,breaks = c(10, 50, 100, 250, 500, 1000, 2000)
                ) +
  scale_color_manual(
    values = c("España" = "orange2",
               "Otros" = "#333333",
               "China" = "#333333",
               "South Korea" = "#333333"
    )
  ) 


# TEMA --------------------------------------------------------------------

p_conf <- p_conf +
  theme(legend.position = "none")

p_muertos <- p_muertos +
  theme(legend.position = "none")


# EXPORTACIÓN DE .PNG -----------------------------------------------------

ggsave("dataviz/confirmados_paises_log.png",
       p_conf, 
       width = 50,
       height = 40, 
       unit = "cm",
       dpi = 320
       )

ggsave("dataviz/muertos_paises_log.png",
       p_muertos, 
       width = 50,
       height = 40, 
       unit = "cm",
       dpi = 320
)