library(ggtext)
library(ggplot2)
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
      color = ifelse(incidence_100k_15d_acum >= 500, ">500", "<500"),
    incidence_100k_15d_acum = floor(incidence_100k_15d_acum))

order_ccaa <- ccaa_labels %>% 
  select(province, incidence_100k_15d_acum) %>% 
  arrange(-incidence_100k_15d_acum)

spain_ccaa$province <- factor(spain_ccaa$province, levels = order_ccaa$province)
ccaa_labels$province <- factor(ccaa_labels$province, levels = order_ccaa$province)

spain_ccaa


segmento <- function(x, cota = 500){
  s <- c(1, numeric(length(x)-1))
  j <- 1
  for (i in 2:length(x)){
    if (x[i] < cota & x[i-1] >= cota) {
      s[i] <- s[i-1] + 1
    } else if (x[i] >= cota & x[i-1] < cota) { 
      s[i] <- s[i-1] + 1
    } else {
      s[i] <- s[i-1]
    }  
  }
  return(s)
  }


spain_ccaa <- spain_ccaa %>% 
  group_by(province) %>% 
  mutate(segmento = segmento(incidence_100k_15d_acum)) %>% 
  mutate(color = ifelse(incidence_100k_15d_acum >= 500, ">500", "<500")) %>% 
  ungroup()


p <- spain_ccaa %>% 
  ggplot(aes(x = date, y = incidence_100k_15d_acum)) + 
  geom_line(color = "#555555",
    size = 3) +
  geom_line(data = spain_ccaa %>% filter(incidence_100k_15d_acum >= 500),
    aes(color = color, group = segmento), 
    size = 3) +
  #geom_line(data = more_500, aes(color = color), size = 3) +
  geom_text(data = ccaa_labels, 
             aes(x = date, label = incidence_100k_15d_acum, color = color), 
             size = 7,
             hjust = "left",
              vjust = "bottom",
             nudge_x = 3) +
  #geom_point(data = ccaa_labels, size = 10, fill = "white", shape = 21) + 
  facet_wrap(.~ province, ncol = 3, scales = "free_x") + 
  labs(
    title = "Incidencia en los últimos 15 días\npor 100.000 habitantes",
    subtitle = "En <span style='color:#e84545;'>rojo</span>, días con incidencia acumulada superior a 500",
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
                                  size = 70, 
                                  family = "Oswald",
                                  margin = margin(r = 50, l = 50, t = 50, b = 30),
                                  hjust = 0.5
        ),
        plot.subtitle = element_markdown(size = 40, 
                                     face = "italic",
                                     family = "Oswald",
                                     margin = margin(b = 50),
                                     hjust = 0.5
        ),
        ) + 
  #scale_color_manual(values = c(">500" = "#CD8500", "<500" = "#555555")) + 
  scale_color_manual(values = c(">500" = "#e84545", "<500" = "#555555")) + 
  scale_y_continuous(breaks = c(0, 500, 1000)) + 
  scale_x_date(expand = expansion(add = c(0,20)), 
               date_labels = "%B",
               breaks = c(as.Date("2020/08/01"), as.Date("2020/10/01")))

ggsave("dataviz/madrid_vs_resto.png", p, width = 40, height = 60, unit = "cm", dpi = 320)

