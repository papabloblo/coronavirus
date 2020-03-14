
# DEPENDENCIAS ------------------------------------------------------------

library(tidyverse)
source("src/tema_ggplot2.R")
theme_set(tema())


# DATOS -------------------------------------------------------------------

daily_reports <- readRDS("data/daily_reports_country.RDS")


# TRATAMIENTO -------------------------------------------------------------

last_data <- daily_reports %>% 
  # filter(country %in% c("Italia", "España", "China")) %>% 
  top_n(1, wt = date) %>% 
  mutate(
    deaths_porc = deaths/confirmed,
    recovered_porc = recovered/confirmed
    )

last_date <- max(daily_reports$date)

last_confirmed_spain <- daily_reports$confirmed[daily_reports$country == "España" &
                                                  daily_reports$date == last_date]

lag_days <- which.min(
  daily_reports$confirmed[daily_reports$country == "Italia"] <= last_confirmed_spain
  ) - 1

date_italy_eq_spain <- daily_reports$date[daily_reports$country == "Italia"][lag_days]


# ITALIA Y ESPAÑA ---------------------------------------------------------

df_italia_esp <- daily_reports %>% 
  filter(country %in% c("Italia", "España"),
         date >= as.Date("2020-02-15"))


p <- df_italia_esp %>% 
  ggplot(
    aes(x = date,
        y = confirmed,
        group = country,
        color = country)
  )

p <- p +
  geom_segment(
    x = date_italy_eq_spain + 0.75, 
    xend = last_date - 0.25,
    y = last_confirmed_spain, 
    yend = last_confirmed_spain,
    color = "#333333",
    alpha = 0.6, 
    size = 1,
    arrow = arrow(ends = "both", type = "closed")
    ) +
  annotate("text",
           x = last_date - 0.25 - (last_date - 0.25 - date_italy_eq_spain - 0.75)/2,
           y = last_confirmed_spain + 500, 
           label = paste((last_date - date_italy_eq_spain), "días"),
           size = 8,
           hjust = "center",
           color = "#333333",
           family = "Oswald"
           )

p <- p +
  geom_line(size = 1.5) +
  geom_text(data = last_data %>% filter(country %in% c("España", "Italia")),
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

p <- p +
  scale_color_manual(
    values = c("España" = "orange2", 
               "Italia" = "#333333"
               )
    ) +
  scale_x_date(
    # date_breaks = "1 week", 
    breaks = c(seq(as.Date("2020-02-17"),
                      last_date,
                      by = "1 week"),
                    last_date), 
    # date_labels = "%d %B", 
    labels = function(x) ifelse(x == last_date,
                                paste0(as.character(x, format = "%d %B"), 
                                       "\n(actualización)"
                                       ),
                                as.character(x, format = "%d %B")),
    expand = expand_scale(add = c(0,3))
    ) +
  scale_y_continuous(
    labels = scales::comma_format(), 
    breaks = seq(0, 15000, by = 2500), 
    minor_breaks = seq(0, 15000, 1250), 
    position = "right"
    )


p <- p +
  labs(
    title = "Evolución de los casos\nconfirmados de coronavirus",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus",
    y = "",
    x = ""
  )

p <- p +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
  )

ggsave("dataviz/italia_vs_espana.png", p, width = 50, height = 40, unit = "cm", dpi = 320)

# CHINA -------------------------------------------------------------------

p2 <- daily_reports %>% 
  filter(country %in% c("China", "Italia"),
         date >= as.Date("2020-02-15")) %>% 
  pivot_longer(cols = confirmed:recovered,
               names_to = "type",
               values_to = "count") %>% 
  mutate(type = factor(type,levels = c("confirmed", "recovered", "deaths"))) %>% 
  group_by(date, country) %>% 
  mutate(count = count/sum(count)) %>% 
  ggplot(
    aes(x = date,
        y = count,
        group = type,
        fill = type)
  ) +
  geom_area() +
  scale_fill_manual(values = c("confirmed" = "orange2", 
                               "deaths" = "#333333",
                               "recovered" = "steelblue")
  ) +
  facet_wrap(~country, ncol = 1) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = scales::percent, position = "right") +
  geom_segment(
    # x = date_italy_eq_spain + 0.25, 
    # xend = unique(last_date$date)-1,
    # y = conf_spain-250, 
    # yend = conf_spain-250,
    x = last_date + 0.5, 
    xend = last_date + 0.5,
    y = 0.25, 
    yend = 1,
    # linetype = "dashed",
    color = "#333333", alpha = 0.6, size = 0.75, arrow = arrow(ends = "both", type = "closed")) +
  labs(
    title = "Casos confirmados, recuperados y muertos",
    x = "",
    y = "",
    aption = "Fuente: Johns Hopkins CSSE. @papabloblog"
  )

ggsave("dataviz/italia_vs_china.png", p2, width = 50, height = 40, unit = "cm", dpi = 320)



p2 <- daily_reports %>% 
  filter(country %in% c("China"),
         date >= as.Date("2020-02-15")) %>% 
  group_by(date, country) %>% 
  mutate(
    recovered = recovered/confirmed,
    deaths = deaths/confirmed
    ) %>% 
  pivot_longer(cols = confirmed:recovered,
               names_to = "type",
               values_to = "count"
               ) %>% 
  mutate(type = factor(type,levels = c("confirmed", "recovered", "deaths"))) %>% 
  filter(type != "confirmed") %>% 
  ggplot(
    aes(x = date,
        y = count,
        group = type,
        fill = type)
  ) +
  geom_area() +
  scale_fill_manual(values = c("confirmed" = "orange2", 
                               "deaths" = "#333333",
                               "recovered" = "steelblue")
  ) +
  # facet_wrap(~country, ncol = 1) +
  theme(legend.position = "none") +
  
  scale_y_continuous(labels = scales::percent, position = "right") +
  labs(
    title = "Porcenaje de casos recuperados y muertos\nrespecto del total de confirmados en China",
    x = "",
    y = "",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus"
  ) + 
  # geom_vline(xintercept = last_date - 2, color = "white", size = 2) + 
  
  geom_rect(
    aes(
    xmin = last_date + 0.25,
    xmax = last_date + 2.25,
    ymin = 0,
    ymax = last_data$recovered_porc[last_data$country == "China"] + 
      last_data$deaths_porc[last_data$country == "China"]
    ),
    fill = "steelblue"
  ) +
  geom_rect(
    aes(
      xmin = last_date + 0.25,
      xmax = last_date + 2.25,
      ymin = 0,
      ymax = last_data$deaths_porc[last_data$country == "China"]
    ),
    fill = "#333333"
  ) +
  annotate(
    "text",
    x = last_date + 1.25, 
    y = .4, 
    label = scales::percent(last_data$recovered_porc[last_data$country == "China"]), 
    color = "white",
    size = 9, 
    family = "Oswald"
  ) +
  annotate(
    "text",
    x = last_date + 1.25, 
    y = .02, 
    label = scales::percent(last_data$deaths_porc[last_data$country == "China"]),
    color = "white",
    size = 8,
    family = "Oswald"
  ) +
  annotate("text", x = last_date - 1, y = .4, label = "Recuperados", color = "white", size = 13, family = "Oswald", hjust = "right") +
  annotate("text", x = last_date - 1, y = .02, label = "Muertos", color = "white", size = 10, family = "Oswald", hjust = "right") +
  annotate(
    "text",
    x = last_date + 1.25, 
    y = last_data$recovered_porc[last_data$country == "China"] + 
      last_data$deaths_porc[last_data$country == "China"] - 0.02, 
    label = "Hoy",
    color = "white",
    size = 13,
    family = "Oswald"
  )

ggsave("dataviz/china.png", p2, width = 50, height = 40, unit = "cm", dpi = 320)





# MIAU --------------------------------------------------------------------

daily_reports_country %>% 
  mutate(pais = ifelse(country == "China", "China", "Otros")) %>%
  group_by(pais, date) %>% 
  summarise(confirmed = sum(confirmed))%>% 
  arrange(date) %>% 
  mutate(confirmed = confirmed - lag(confirmed)) %>% 
  filter(  date >= as.Date("2020-02-15")) %>% 
  ggplot(
    aes(x = date,
        y = confirmed,
        group = pais,
        color = pais)
  ) +
  geom_line(size = 1.5)
geom_text(data = last_date,
          aes(x = date, y = confirmed, label = country),
          nudge_x = 0.15, nudge_y = 150, size = 10, hjust = "left",family = "Oswald") +
  annotate("text",
           x = unique(last_date$date)- 0.25 - (unique(last_date$date)-0.25 - date_italy_eq_spain+0.25)/2,
           y = conf_spain + 250, label = "Diferencia de 9 días",
           size = 8, hjust = "center", color = "#333333", family = "Oswald") +
  
  scale_color_manual(values = c("España" = "orange2", 
                                "Italia" = "#333333")
  ) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %B", expand = expand_scale(add = c(0,3))) +
  scale_y_continuous(labels = scales::comma_format(), breaks = seq(0, 10000, by = 2500), minor_breaks = seq(0, 10000, 1250), position = "right") +
  labs(
    title = "Evolución de los casos\nconfirmados de coronavirus",
    # subtitle = "Casos confirmados",
    caption = "Fuente: Johns Hopkins CSSE. @papabloblog",
    y = "",
    x = ""
  ) +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
  )
