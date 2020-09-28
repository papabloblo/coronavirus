tema <- function(){
  theme_minimal() +
    theme(
      text = element_text(color = "#333333"),
      plot.caption = element_text(family = "Oswald"),
      panel.grid.minor = element_blank(),
      
      axis.text.y = element_text(hjust = 0, family = "Oswald", size = 30),
      axis.text.x.top = element_text(hjust = 0, family = "Oswald", size = 30),
      axis.text.x = element_text(family = "Oswald", size = 40),
      
      strip.text = element_text(angle = 0, 
                                hjust = 0,
                                size = 20,
                                family = "Oswald",
                                face = "bold",
                                margin = margin(t = 5, b = 1),
                                color = "#333333"
      ),
      
      plot.title = element_text(face = "bold",
                                size = 50, 
                                family = "Oswald",
                                margin = margin(b = 10),
                                hjust = 0.5
      ),
      plot.subtitle = element_text(size = 15, 
                                   face = "italic",
                                   family = "Oswald",
                                   margin = margin(b = 20),
                                   hjust = 0.5
      ),
      # panel.grid.minor = element_blank(),
      # axis.text.y = element_blank(),
      
      # panel.grid.major.y  = element_blank(),
      # strip.text.y = element_text(angle = 180, hjust = 1, size = 12, family = "Oswald", face = "bold"),
      
      panel.spacing  = unit(1, "cm")
      
      
    )
}