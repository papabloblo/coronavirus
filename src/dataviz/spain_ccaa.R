
library(tidyverse)

source("src/dataviz/theme_ggplot.R")

spain_ccaa <- readRDS("data/01_tratamiento/country_province.RDS")

spain_ccaa <- spain_ccaa %>% 
  filter(country == "Spain", date > as.Date("2020-06-01"))

ccaa_highlight <- spain_ccaa %>% 
  filter(province %in% c("Madrid", "Aragon", "Navarra"))

ccaa_colors <- c("Madrid" = "firebrick", "Aragon" = "orange3", "Navarra" = "steelblue")

ccaa_labels <- ccaa_highlight %>% 
  group_by(country, province) %>% 
  top_n(1, date) %>% 
  ungroup()
  

p <- spain_ccaa %>% 
  ggplot(aes(x = date, y = incidence_100k_15d_acum, group = province)) + 
  geom_line(alpha = 0.25) +
  geom_line(data = ccaa_highlight, 
            aes(color = province),
            size = 5
            ) +
  scale_color_manual(values = ccaa_colors) +
  scale_y_continuous(position = "right") +
  scale_x_date(expand = expand_scale(add = c(0,10)), 
               date_labels = "%B")+
  geom_label(data = ccaa_labels, 
             aes(x = date, label = province, color = province), 
             size = 12,
             hjust = "center",
             label.padding = unit(0.5, "lines"),
             label.size = 1,
             label.r = unit(0, "lines"),
             nudge_x = 5) +
  labs(
    title = "Incidencia acumulada en los 15 días anteriores\npor cada 100.000 habitantes",
    x = "",
    y = "",
    caption = "Datos: github.com/CSSEGISandData/COVID-19\n@papabloblog"
  ) +
  tema() + 
  theme(
    legend.position = "none"
  )


ggsave("dataviz/spain_ccaa.png", p, width = 60, height = 40, unit = "cm", dpi = 320)
