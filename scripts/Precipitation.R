# Load required packages
library(tidyverse)


## Create Plotting theme
theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 22, color = "black"),
      axis.title = element_text(size = 22, color = "black"),
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


######

# Load data
precip_data <- read_csv("data/ClimateEngine.csv")

# Long-term mean
long_term_avg <- mean(precip_data$annual_precipitation_mm, na.rm = TRUE)

# Plot
annual_plot <- ggplot(precip_data, aes(x = year, y = annual_precipitation_mm)) +
  
  # Uniform bar colour
  geom_col(
    fill = "steelblue",
    colour = "black",
    linewidth = 0.3,
    width = 0.8
  ) +
  
  # Long-term mean
  geom_hline(
    yintercept = long_term_avg,
    linetype = "dashed",
    colour = "black",
    linewidth = 0.6
  ) +
  
  # X-axis labelled at intervals only
  scale_x_continuous(
    breaks = seq(min(precip_data$year),
                 max(precip_data$year),
                 by = 5)
  ) +
  
  labs(
    x = "Year",
    y = "Annual precipitation (mm)"
  ) +
  theme_beautiful()

annual_plot

ggsave(
  annual_plot,  
  filename = "annual_figure.png",  
  width = 26, height = 16,  
  units = "cm"
)

ggsave(
  filename = "annual_figure.png",
  plot = annual_plot,
  width = 26,
  height = 16,
  units = "cm",
  dpi = 300
)
