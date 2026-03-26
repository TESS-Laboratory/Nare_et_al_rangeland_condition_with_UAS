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
precip_data <- read_csv("data/chirps_precipitation_2000_2024.csv")

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



################################################################################
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

# Load CSV
rain_data <- read_csv("data/chirps_precipitation_2000_2024.csv", col_types = cols(
  date = col_character(),
  rainfall = col_double()
))

# Parse dates
rain_data <- rain_data %>%
  mutate(date = dmy(date))   # adjust if your CSV format is different

# Extract year and month
rain_data <- rain_data %>%
  mutate(
    year = year(date),
    month = month(date, label = TRUE, abbr = TRUE)  # "Jan", "Feb", etc.
  )

# --- Step 1: Calculate total rainfall per calendar month ---
monthly_totals <- rain_data %>%
  group_by(year, month) %>%
  summarise(monthly_rainfall = sum(rainfall, na.rm = TRUE)) %>%
  ungroup()

# --- Step 2: Calculate long-term average for each calendar month ---
long_term_avg <- monthly_totals %>%
  group_by(month) %>%
  summarise(avg_rainfall = mean(monthly_rainfall, na.rm = TRUE)) %>%
  ungroup()

# --- Step 3: Filter recent months of interest (Oct 2023 – Mar 2024) ---
months_of_interest <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")

# Step 3: Filter recent months and create ordered factor for plotting
rain_recent <- monthly_totals %>%
  filter(month %in% months_of_interest & 
           ((year == 2023 & month %in% c("Oct","Nov","Dec")) |
              (year == 2024 & month %in% c("Jan","Feb","Mar")))) %>%
  mutate(month_year = paste(month, year)) %>%
  # Make month_year an ordered factor to ensure correct plotting order
  mutate(month_year = factor(month_year, levels = c(
    "Oct 2023", "Nov 2023", "Dec 2023", "Jan 2024", "Feb 2024", "Mar 2024"
  ))) %>%
  left_join(long_term_avg, by = "month")
# --- Step 4: Plot ---
sup_rain <- ggplot(rain_recent, aes(x = month_year)) +
  geom_col(aes(y = monthly_rainfall), fill = "skyblue", width = 0.6) +   # actual rainfall
  geom_point(aes(y = avg_rainfall), color = "red", size = 3) +           # long-term average
  geom_line(aes(y = avg_rainfall, group = 1), color = "red", linetype = "dashed") +
  labs(
    x = "Month",
    y = "Rainfall (mm)"
  ) +
  theme_beautiful() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5)  # horizontal, centered under tick
  )

sup_rain


ggsave("Supplementary_rainfall.tiff", 
       sup_rain, 
       width = 10, 
       height = 5, 
       dpi = 600, 
       device = "tiff", 
       compression = "lzw")

