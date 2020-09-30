
madrid <- spain_ccaa %>% 
  filter(province == "Madrid") %>% 
  select(date, incidence_100k_15d_acum)

spain_ccaa <- spain_ccaa %>% 
  filter(province != "Unknown")


ccaa_labels <- spain_ccaa %>% 
  group_by(country, province) %>% 
  top_n(1, date) %>% 
  ungroup() %>% 
  mutate(
    color = ifelse(province == "Madrid", "Madrid", "Otras"),
    incidence_100k_15d_acum = floor(incidence_100k_15d_acum))

order_ccaa <- ccaa_labels %>% 
  select(province, incidence_100k_15d_acum) %>% 
  arrange(-incidence_100k_15d_acum)

spain_ccaa$province <- factor(spain_ccaa$province, levels = order_ccaa$province)
ccaa_labels$province <- factor(ccaa_labels$province, levels = order_ccaa$province)


p <- spain_ccaa %>% 
  mutate(color = ifelse(province == "Madrid", "Madrid", "Otras")) %>% 
  ggplot(aes(x = date, y = incidence_100k_15d_acum)) + 
  geom_line(aes(color = color), size = 3) +
  geom_text(data = ccaa_labels, 
             aes(x = date, label = incidence_100k_15d_acum, color = color), 
             size = 7,
             hjust = "left",
              vjust = "bottom",
             nudge_x = 3) +
  facet_wrap(.~ province, ncol = 3, scales = "free_x") + 
  labs(
    title = "Incidencia en los últimos 15 días\npor 100.000 habitantes",
    x = "",
    y = ""
    ) +
  tema() + 
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(hjust = 0, family = "Oswald", size = 20),
        axis.text.x.top = element_text(hjust = 0, family = "Oswald", size = 30),
        axis.text.x = element_text(family = "Oswald", size = 20),
        plot.title = element_text(face = "bold",
                                  size = 80, 
                                  family = "Oswald",
                                  margin = margin(t = 50, b = 80),
                                  hjust = 0.5
        )
        ) + 
  scale_color_manual(values = c("Madrid" = "orange3", "Otras" = "#555555")) + 
  scale_y_continuous(breaks = c(0, 500, 1000)) + 
  scale_x_date(expand = expand_scale(add = c(0,20)), 
               date_labels = "%B",
               breaks = c(as.Date("2020/08/01"), as.Date("2020/10/01")))

ggsave("dataviz/madrid_vs_resto.png", p, width = 40, height = 60, unit = "cm", dpi = 320)

