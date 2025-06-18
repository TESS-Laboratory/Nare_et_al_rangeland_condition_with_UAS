# Load required libraries
library(tidyverse)

## Create Plotting theme
theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly



# Create data frame from your thesis data with altitudes ordered from lowest to highest
altitude_data <- data.frame(
  altitude = c(10, 20, 25, 30, 35, 40, 50, 70, 75, 80, 100, 110, 115, 120, 140),
  percent = c(8, 14, 3, 16, 2, 8, 23, 3, 1, 2, 6, 1, 1, 10, 2)
)

# Ensure altitude is a factor with levels ordered from lowest to highest
altitude_data <- altitude_data %>%
  mutate(altitude = factor(altitude, levels = sort(unique(altitude))))

# Create bar plot
a <- ggplot(altitude_data, aes(x = altitude, y = percent)) +
  geom_bar(stat = "identity", fill = "#4e79a7", width = 0.6) +
  geom_text(aes(label = paste0(percent, "%")), 
            vjust = -0.5, size = 3.5, color = "black") +
  labs(
    x = "Altitude (m)",
    y = "Percentage of Studies (%)"
  ) +
  theme_classic(base_size = 12) +
  theme_beautiful()+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black")
  )

# Display plot
a

## NB. if you want to combine panels in a figure, R's 'patchework' (https://patchwork.data-imaginist.com/) package is very powerful and easy to use, with easy panel labels.


## ggsave call (use this after you've defined you plot objects)
## NB. Raster-type files (e.g., the ".png" format is fine for inclusion in a Word document for a dissertation, thesis or manuscript for review.
## for final production of papers, it's usually preferable to submit vectorised figures e.g. ".pdf", easly switched by changing the file extention in the ggsave call). 

ggsave(
  a,  
  filename = "alti.png",  # use relative rather than absolute file paths.
  width = 16, height = 16,  # adjust these based on the intended display size - Portrait A4 documents typically have a ca. 16 cm wide display area.
  units = "cm"
)
