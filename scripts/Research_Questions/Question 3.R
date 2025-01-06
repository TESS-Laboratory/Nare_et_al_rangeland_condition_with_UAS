# Title: Evaluating model performance and transferability across degradation gradients
# Author: Alan Nare and Andrew Cunliffe

# Load necessary libraries
library(tidyverse)
library(performance)
library(sjPlot)
library(effects)
library(patchwork)
library(pracma)
library(report)
library(olsrr)
library(robustbase)

# Read the data
ND <- read_csv("C:/Workspace/Nare_et_al_rangeland_condition_with_UAS/data/Data.csv")
head(ND)

# Step 1: Fit models ----
# Perform robust regression for NDVI vs AGB
robust_ndvi <- robustbase::lmrob(`AGB (g/m2)` ~ NDVI, data=ND)
summary(robust_ndvi)
tab_model(robust_ndvi)

# Perform robust regression for Canopy height vs AGB
robust_chm <- robustbase::lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data=ND)
summary(robust_chm)
tab_model(robust_chm)
plot_residuals(robust_chm)
performance(robust_chm)

# Plot Predicted vs Observed AGB (Canopy Height Model)
plot(x = predict(robust_chm), y = ND$`AGB (g/m2)`,
     xlab = "Predicted values",
     ylab = "Observed values",
     main = "Predicted vs observed")
abline(a = 0, b = 1, col = "black", lty = 30)  # Add the abline here

# Step 2: Get the predicted values
ND$PredictedCHM <- predict(robust_chm)

# Step 3: Save the predicted values and other relevant columns to a CSV
write_csv(ND, "C:/Workspace/Nare_et_al_rangeland_condition_with_UAS/predicted_values.csv")

# Read the saved CSV file with predicted values
Pred <- read_csv("C:/Workspace/Nare_et_al_rangeland_condition_with_UAS/predicted_values.csv")

# Step 4: Create new data frames for each AOI ----

new_data_aoi1 <- Pred %>%                                                             # High grazing intensity AOI
  select(Species, `Grazing value`, `AGB (g/m2)`, `Mean_Canopy_Height(m)`, NDVI, PredictedCHM) %>% 
  filter(`AOI` == "1")

new_data_aoi2 <- Pred %>%                                                             # Average grazing intensity AOI
  select(Species, `Grazing value`, `AGB (g/m2)`, `Mean_Canopy_Height(m)`, NDVI, PredictedCHM) %>% 
  filter(`AOI` == "2")

new_data_aoi3 <- Pred %>%                                                             # Low grazing intensity AOI
  select(Species, `Grazing value`, `AGB (g/m2)`, `Mean_Canopy_Height(m)`, NDVI, PredictedCHM) %>% 
  filter(`AOI` == "3")


# Step 5: Calculate performance metrics ----
# Function to calculate RMSE, MAE, R-squared, and mean residuals
calc_metrics <- function(observed, predicted) {
  rmse <- sqrt(mean((observed - predicted)^2))
  mae <- mean(abs(observed - predicted))
  r_squared <- cor(observed, predicted)^2
  mean_residual <- mean(observed - predicted)
  return(list(RMSE = rmse, MAE = mae, R2 = r_squared, Mean_Residual = mean_residual))
}

# Calculate metrics for each AOI for Canopy Height model
metrics_aoi1_chm <- calc_metrics(new_data_aoi1$`AGB (g/m2)`, new_data_aoi1$PredictedCHM)
metrics_aoi2_chm <- calc_metrics(new_data_aoi2$`AGB (g/m2)`, new_data_aoi2$PredictedCHM)
metrics_aoi3_chm <- calc_metrics(new_data_aoi3$`AGB (g/m2)`, new_data_aoi3$PredictedCHM)

# Print metrics for each AOI
metrics_aoi1_chm
metrics_aoi2_chm
metrics_aoi3_chm

# Create Plotting theme ----
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
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"),
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

# Ensure font is mapped correctly
windowsFonts("Helvetica" = windowsFont("Helvetica"))

# Step 6: Plot PredictedCHM vs AGB for each AOI ----
plot_aoi1_chm <- ggplot(new_data_aoi1, aes(x = PredictedCHM, y = `AGB (g/m2)`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(title = "Predicted vs Observed AGB (AOI 1)", x = "Predicted AGB (g/m2)", y = "Observed AGB (g/m2)") +
  theme_beautiful()

plot_aoi2_chm <- ggplot(new_data_aoi2, aes(x = PredictedCHM, y = `AGB (g/m2)`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(title = "Predicted vs Observed AGB (AOI 2)", x = "Predicted AGB (g/m2)", y = "Observed AGB (g/m2)") +
  theme_beautiful()

plot_aoi3_chm <- ggplot(new_data_aoi3, aes(x = PredictedCHM, y = `AGB (g/m2)`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  labs(title = "Predicted vs Observed AGB (AOI 3)", x = "Predicted AGB (g/m2)", y = "Observed AGB (g/m2)") +
  theme_beautiful()

# Combine plots into one
combined_plot <- plot_aoi1_chm + plot_aoi2_chm + plot_aoi3_chm + 
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'a')  # Adding tags for the plots

# Display the combined plot
print(combined_plot)

# Save the plot
ggsave(
  combined_plot,  # Use the combined plot object
  filename = "Plots/Figure_comparison_chm.png",  
  width = 16, height = 16,  # Adjust the size as needed
  units = "cm"
)

