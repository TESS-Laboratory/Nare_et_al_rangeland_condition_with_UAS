# Code for analysing the relationship between AGB and UAV derived Canopy Height,
# and Spectral reflectance (NDVI)
# Author: Alan Nare (alandnare@gmail.com) and Andrew Cunliffe
# Purpose: To perform robust regression on AGB and NDVI / Mean Canopy Height, and visualise the results

# Load necessary libraries
library(tidyverse)      # Collection of packages for data manipulation and visualization (ggplot2, dplyr, etc.)
library(robustbase)     # For robust regression methods (lmrob)
library(olsrr)          # For OLS regression diagnostics (e.g., residual plots)
library(patchwork)      # For combining multiple ggplots into one plot
library(sjPlot)

# Read the data
ND <- read_csv("data/Data.csv")

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

# View data structure (first few rows and structure of the data)
head(ND)
str(ND)
view(ND)
#####Visualising data distribution
a <- ggplot(ND, aes(x = Mean_Canopy_Height_m)) +
  geom_density()+
  theme_beautiful()+
  labs(x = "Mean Canopy Height (m)")

b <- ggplot(ND, aes(x = NDVI)) +
  geom_density()+
  theme_beautiful()

c <- ggplot(ND, aes(x = AGB_g_m_2)) +
  geom_density()+
  theme_beautiful()

combined_plot <- (a + b + c) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a')
combined_plot
ggsave("combined_plot.jpg", height = 16, width = 35, units = "cm")
view(ND)

# Convert AOI and Species columns to factors (for categorical variables)
ND$AOI <- factor(ND$AOI)
ND$Species <- factor(ND$Species)

# Perform OLS regression (Ordinary Least Squares)
m <- lm(AGB_g_m_2 ~ NDVI, data=ND)  # OLS model for AGB vs NDVI
n <- lm(AGB_g_m_2 ~ Mean_Canopy_Height_m, data=ND)  # OLS model for AGB vs Mean Canopy Height


# Print summaries and reports of the OLS models
summary(n)
summary(m)

# Create tables of model results
tab_model(n)
tab_model(m)

# Generate the plot using ols_plot_resid_lev
resid_lev_plotchm <- ols_plot_resid_lev(n)

# Apply theme beautiful
resid_lev_plotchm + theme_beautiful()

# Generate the plot using ols_plot_resid_lev
resid_lev_plotndvi <- ols_plot_resid_lev(m)

# Apply theme beautiful
resid_lev_plotndvi + theme_beautiful()
ggsave("combined.jpg", height = 16, width = 16, units = "cm")




# Perform robust regression (lmrob) for both models
rm <- robustbase::lmrob(AGB_g_m_2 ~ NDVI, data=ND)  # Robust regression for AGB vs NDVI
rn <- robustbase::lmrob(AGB_g_m_2 ~ Mean_Canopy_Height_m, data=ND)  # Robust regression for AGB vs Canopy Height

# Print summaries and tables for robust regression models
summary(rm)
tab_model(rm)
summary(rn)
tab_model(rn)


# Extract coefficients and p-values for robust regression lines
intercept_rn <- coef(rn)[1]
slope_rn <- coef(rn)[2]
p_value_rn <- summary(rn)$coefficients[2, "Pr(>|t|)"]
equation_rn <- paste0("AGB = ", round(intercept_rn, 2), " + ", round(slope_rn, 2), " * Mean Canopy Height (m)")
details_rn <- paste0("p-value = ", format.pval(p_value_rn, digits = 2), "\nn = ", nrow(ND))


# Define a custom color palette for species
custom_colors <- c(
  "S.kalihariensis" = "#1f77b4",  # Blue
  "S.pappophoroides" = "#ff7f0e",  # Orange
  "S.uniplumis" = "#2ca02c",  # Green
  "S.ciliata" = "#d62728",  # Red
  "T.terrestris" = "#9467bd",  # Purple
  "E.pallens" = "#FFF000"    # Yellow
)

# Plot a: Mean Canopy Height vs AGB with robust regression line
plota <- ND %>%
  ggplot(aes(x = Mean_Canopy_Height_m, y = AGB_g_m_2)) +
  geom_point(alpha = 0.6) +  # Scatter plot points
  geom_abline(intercept = intercept_rn, slope = slope_rn) +  # Add robust regression line
  theme_beautiful() +  # Use a classic theme
  labs(
    x = "Mean Canopy Height (m)",
    y = "Above Ground Biomass (g/m²)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  annotate("text", x = min(ND$Mean_Canopy_Height_m), y = max(ND$AGB_g_m_2) - 0.05 * max(ND$AGB_g_m_2),
           label = equation_rn, hjust = 0, vjust = 1, size = 4, color = "black") +
  annotate("text", x = min(ND$Mean_Canopy_Height_m), y = max(ND$AGB_g_m_2) - 0.15 * max(ND$AGB_g_m_2),
           label = details_rn, hjust = 0, vjust = 1, size = 4, color = "black") +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(tag = "a)") +
  ylim(0, 1000)  # Set y-axis limits

# Print the plot
print(plota)

# Plot b: Robust regression lines for each species
plotb <- ND %>%
  ggplot(aes(x = Mean_Canopy_Height_m, y = AGB_g_m_2, color = Species)) +
  geom_point(alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lmrob", formula = y ~ x, se = FALSE) +  # Add robust regression line for each species
  theme_beautiful() +  # Use a classic theme
  labs(
    x = "Mean Canopy Height (m)",
    y = "Above Ground Biomass (g/m²)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(tag = "b)") +
  ylim(0, 1000)  # Set y-axis limits

# Print the plot
print(plotb)


############### For NDVI vs AGB ###############################----

# Extract coefficients for the robust regression line (NDVI vs AGB)
intercept_rm <- coef(rm)[1]
slope_rm <- coef(rm)[2]
p_value_rm <- summary(rm)$coefficients[2, "Pr(>|t|)"]
equation_rm <- paste0("AGB = ", round(intercept_rm, 2), " + ", round(slope_rm, 2), " * NDVI")
details_rm <- paste0("p-value = ", format.pval(p_value_rm, digits = 2), "\nn = ", nrow(ND))

# Plot c: NDVI vs AGB with robust regression line
plotc <- ND %>%
  ggplot(aes(x = NDVI, y = AGB_g_m_2)) +
  geom_point(alpha = 0.6) +  # Scatter plot points
  geom_abline(intercept = intercept_rm, slope = slope_rm) +  # Add robust regression line
  theme_beautiful() +  # Use a classic theme
  labs(
    x = "NDVI",
    y = "Above Ground Biomass (g/m²)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  annotate("text", x = min(ND$NDVI), y = max(ND$AGB_g_m_2) - 0.05 * max(ND$AGB_g_m_2),
           label = equation_rm, hjust = 0, vjust = 1, size = 4, color = "black") +
  annotate("text", x = min(ND$NDVI), y = max(ND$AGB_g_m_2) - 0.15 * max(ND$AGB_g_m_2),
           label = details_rm, hjust = 0, vjust = 1, size = 4, color = "black") +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(tag = "c)") +
  ylim(0, 1000)  # Set y-axis limits

# Print the plot
print(plotc)

# Plot d: Robust regression lines for each species without model annotations for NDVI
plotd <- ND %>%
  ggplot(aes(x = NDVI, y = AGB_g_m_2, color = Species)) +
  geom_point(alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lmrob", formula = y ~ x, se = FALSE) +  # Add robust regression line for each species
  theme_beautiful() +  # Use a classic theme
  labs(
    x = "NDVI",
    y = "Above Ground Biomass (g/m²)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)  # Center the plot title
  ) +
  scale_color_manual(values = custom_colors) +  # Apply custom colors
  labs(tag = "d)") +
  ylim(0, 1000)  # Set y-axis limits

# Print the plot
print(plotd)


# Combine plots for side-by-side visualization
combined_plot <- (plota + plotb) / (plotc + plotd) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a')

# Display combined plot
print(combined_plot)

# Save the combined plot using ggsave

ggsave("combined.jpg", height = 28, width = 28, units = "cm")

