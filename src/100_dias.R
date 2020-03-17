library(tidyverse)

daily_reports <- readRDS("data/daily_reports_country.RDS")

daily_reports <- daily_reports %>% 
  filter(!(is.na(date_100)),
         date >= date_100, 
         country %in% c("España", "Italia", "South Korea","France", "Iran", "Germany")
         ) %>% 
  mutate(
    country = case_when(
      country == "France" ~ "Francia",
      country == "South Korea" ~ "Corea\ndel Sur",
      country == "Iran" ~ "Irán",
      country == "Germany" ~ "Alemania",
      TRUE ~ country
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





p <- daily_reports %>% 
  # filter(
  #   
  #        !(country %in% c("Iran (Islamic Republic of)",
  #                         "Republic of Korea", 
  #                         "Korea, South")),
  #          country %in% c("España", "Italia", "South Korea", "France", "Iran")
  #        ) %>% 
  # filter(dias %% 3 == 0) %>% 
  filter(country != "España") %>% 
  ggplot(
    aes(
      x = dias,
      y = confirmed,
      group = country
    )
  ) + 
  geom_line() +
  geom_line(data = daily_reports %>% filter(country == "España"), 
            aes(color = country), 
            size = 3) +
  geom_label(data =
              daily_reports %>%
              filter(
                date == last_date(),
                # country2 != "Otros",
                # country %in% c("España", "Italia", "South Korea","France", "Iran", "Germany")
              ),
            aes(
              y = confirmed,
              label = country,
              color = country2
            ),
            # nudge_x = 0.15,
            # nudge_y = 150,

            size = 10,

            hjust = "center",
            family = "Oswald",
            label.r = unit(0, "lines"),
  ) +
  labs(
    title = "Evolución después de los\n100 primeros casos confirmados",
    x = "",
    y = "",
    caption = "Fuente: Johns Hopkins CSSE.\ngithub.com/papabloblo/coronavirus"
  ) +
  scale_x_continuous(breaks = seq(0, 25, by = 2), 
                     labels = function(x) ifelse(x == 2,
                                                 paste("+", x, "días"),
                                                 x)
                     ) +
  # scale_y_log10(position = "right",
  #               labels = scales::comma_format(),
  #               breaks = c(100, 500, 1000, 2500, 5000, 20000)) +
  scale_color_manual(
    values = c("España" = "orange2",
               "Otros" = "#333333",
               "China" = "#333333",
               "South Korea" = "#333333"
    )
  ) +
  theme(legend.position = "none")


ggsave("dataviz/espanya_log.png",
       p, 
       width = 50,
       height = 40, 
       unit = "cm",
       dpi = 320
)

# 
# daily_reports %>% 
#   filter(
#     
#     !(country %in% c("Iran (Islamic Republic of)",
#                      "Republic of Korea", 
#                      "Korea, South")),
#     country %in% c("España", "Italia", "South Korea", "France", "Iran")
#   ) %>% 
#   ggplot(
#     aes(
#       x = dias,
#       y = confirmed
#     )
#   ) + 
#   geom_point(alpha = 0.6, size = 5)
