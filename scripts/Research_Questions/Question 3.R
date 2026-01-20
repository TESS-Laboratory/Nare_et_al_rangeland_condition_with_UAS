# Title: To evaluate whether the relationships between AGB and UAV-derived 
# metrics (canopy height and NDVI) varied across grazing intensity levels (AOI), 
# we modelled AGB as a function of NDVI, canopy height, and their interactions 
# with grazing intensity using robust linear regression. Marginal effects were 
# visualised by predicting AGB across observed NDVI or canopy height ranges while 
# holding the other variable at its median, stratified by AOI.

# Author: Alan Nare and Andrew Cunliffe

# Load necessary libraries
library(lme4)
library(tidyverse)
library(performance)
library(sjPlot)
library(effects)
library(patchwork)
library(pracma)
library(report)
library(olsrr)
library(robustbase)
library(car)
library(emmeans)
library(MASS)
## Create Plotting theme

## Create Plotting theme
theme_beautiful <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 12, color = "black"),
      axis.title = element_text(size = 14, color = "black"),
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

# Read the data
ND <- read_csv("data/Data.csv")
head(ND)
ND$AOI <- as.factor(ND$AOI)

# 
model_robust <- MASS::rlm(AGB_g_m_2 ~ NDVI * AOI + Mean_Canopy_Height_m * AOI, data = ND)
model_robust <- lm(AGB_g_m_2 ~ NDVI * AOI + Mean_Canopy_Height_m * AOI, data = ND)
summary(model_robust)
confint(model_robust)
###


# Generate a grid for NDVI predictions
ndvi_seq <- seq(min(ND$NDVI), max(ND$NDVI), length.out = 100)
aoi_levels <- levels(factor(ND$AOI))
new_data_ndvi <- expand.grid(
  NDVI = ndvi_seq,
  Mean_Canopy_Height_m = median(ND$Mean_Canopy_Height_m, na.rm = TRUE),
  AOI = aoi_levels
)
new_data_ndvi$predicted <- predict(model_robust, newdata = new_data_ndvi)
view(new_data_ndvi)
ndvi_pred_plot <- ggplot(ND, aes(x = NDVI, y = AGB_g_m_2, color = AOI)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data_ndvi, aes(x = NDVI, y = predicted, color = AOI), size = 1.2) +
  labs(x = "NDVI",
    y = "Aboveground Biomass (g/m²)",
    color = "AOI"
  ) +
  theme_beautiful()

## Canopy height
# Generate a sequence of canopy height values
chm_seq <- seq(min(ND$Mean_Canopy_Height_m, na.rm = TRUE), 
               max(ND$Mean_Canopy_Height_m, na.rm = TRUE), 
               length.out = 100)

# Create a new data frame for prediction
new_data_chm <- expand.grid(
  Mean_Canopy_Height_m = chm_seq,  
  NDVI = median(ND$NDVI, na.rm = TRUE),  # Hold NDVI constant
  AOI = aoi_levels  # All AOI levels
)

# Predict AGB using the robust model
new_data_chm$predicted <- predict(model_robust, newdata = new_data_chm)


chm_pred_plot <- ggplot(ND, aes(x = Mean_Canopy_Height_m, y = AGB_g_m_2, color = AOI)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data_chm, aes(x = Mean_Canopy_Height_m, y = predicted, color = AOI), size = 1.2) +
  labs(    x = "Mean Canopy Height (m)",
    y = "Aboveground Biomass (g/m²)",
    color = "AOI"
  ) +
  theme_beautiful()


combined_plot <- chm_pred_plot + ndvi_pred_plot +
  plot_annotation(tag_levels = 'a') &
  theme(plot.tag = element_text(face = "bold"))
combined_plot

# Save the combined plot
ggsave("pred_aoi.png", 
       combined_plot, 
       width = 10, 
       height = 5, 
       dpi = 300)



#### Model statistics
ndvi_slopes <- emtrends(
  model_robust,
  specs = ~ AOI,
  var = "NDVI"
)
summary(ndvi_slopes, infer = TRUE)


chm_slopes <- emtrends(
  model_robust,
  specs = ~ AOI,
  var = "Mean_Canopy_Height_m"
)
summary(chm_slopes, infer = TRUE)


