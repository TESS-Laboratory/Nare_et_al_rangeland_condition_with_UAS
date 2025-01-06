# Title: Robust Regression Analysis of Herbaceous Biomass Prediction Using UAV Observations Across Grazing Value Categories

# Description:
# This R code conducts a robust regression analysis to predict herbaceous biomass (AGB) 
# using UAV-derived observations of canopy height and NDVI (spectral reflectance) for 
# different grazing value categories (Low, Average, High). The data is grouped by grazing value, 
# and robust regression models are fitted for each group. Model performance is evaluated using 
# metrics such as R², adjusted R², RMSE, Sigma (standard deviation of residuals), 
# and a composite performance score. The results are summarized and compared across grazing 
# value categories to identify the most effective predictors of AGB for different herbaceous species 
# based on grazing intensity.

# By: Alan Nare and Andrew Cunliffe

# Load required libraries
library(tidyverse)
library(robustbase)
library(performance)
library(sjPlot)
library(patchwork)
# Load your data
# Assuming ND is your data frame containing the columns for Grazing value, NDVI, Mean Canopy Height, and AGB
ND <- read_csv("C:/Workspace/Nare_et_al_rangeland_condition_with_UAS/data/Data.csv")
head(ND)

# Convert AOI and Species columns to factors (for categorical variables)
ND$AOI <- factor(ND$AOI)
ND$Species <- factor(ND$Species)

# Grouping the data by Grazing value
Low <- ND %>%
  select(Species, `Grazing value`, `AGB (g/m2)`, `Mean_Canopy_Height(m)`, NDVI) %>% 
  filter(`Grazing value` == "Low")


Average <- ND %>% 
  select(Species, `Grazing value`, `AGB (g/m2)`, `Mean_Canopy_Height(m)`, NDVI) %>% 
  filter(`Grazing value` == "Average")


High <- ND %>% 
  select(Species, `Grazing value`, `AGB (g/m2)`, `Mean_Canopy_Height(m)`, NDVI) %>% 
  filter(`Grazing value` == "High")

# Function to fit robust regression and evaluate performance
perform_robust_regression <- function(data, grazing_value) {
  # Fit a robust regression model
  model <- lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data = data)
  
  # Model performance metrics
  model_perf <- model_performance(model)
  
  # Print performance metrics
  cat("\nPerformance for Grazing Value: ", grazing_value, "\n")
  print(model_perf)
  
  return(model_perf)
}

# List to store performance results
performance_results <- list()

# Perform robust regression and evaluate performance for each grazing value category
performance_results$Low <- perform_robust_regression(Low, "Low")
performance_results$Average <- perform_robust_regression(Average, "Average")
performance_results$High <- perform_robust_regression(High, "High")

# 

##########################################################

# Perform robust regression (lmrob) for both models
rlowNDVI <- robustbase::lmrob(`AGB (g/m2)` ~ NDVI, data=Low)  
rlowCHM <- robustbase::lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data=Low)  

ravNDVI <- robustbase::lmrob(`AGB (g/m2)` ~ NDVI, data=Average)  
ravCHM <- robustbase::lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data=Average)

rhighNDVI <- robustbase::lmrob(`AGB (g/m2)` ~ NDVI, data=High)
rhighCHM <- robustbase::lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data=High)


# Print summaries and tables for robust regression models
summary(rlowNDVI)
tab_model(rlowNDVI)
summary(rlowCHM)
tab_model(rlowCHM)


summary(ravNDVI)
tab_model(ravNDVI)
summary(ravCHM)
tab_model(ravCHM)


summary(rhighNDVI)
tab_model(rhighNDVI)
summary(rhighCHM)
tab_model(rhighCHM)



# Models
models <- list(
  rlowNDVI = lmrob(`AGB (g/m2)` ~ NDVI, data=Low),
  rlowCHM = lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data=Low),
  ravNDVI = lmrob(`AGB (g/m2)` ~ NDVI, data=Average),
  ravCHM = lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data=Average),
  rhighNDVI = lmrob(`AGB (g/m2)` ~ NDVI, data=ND),
  rhighCHM = lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data=ND)
)

# Corresponding data for each model
data_list <- list(Low, Low, Average, Average, ND, ND)

# Function to perform LOOCV and compute the relative error
compute_relative_error <- function(model, data) {
  n <- nrow(data)
  predictions <- numeric(n)
  
  for (i in 1:n) {
    train_data <- data[-i, ]
    test_data <- data[i, , drop = FALSE]
    
    # Refit the model on the training data
    refit_model <- lmrob(formula(model), data = train_data)
    
    # Predict on the test data
    predictions[i] <- predict(refit_model, newdata = test_data)
  }
  
  # Calculate the mean out-of-sample prediction error
  actuals <- data$`AGB (g/m2)`
  prediction_error <- mean((predictions - actuals)^2)
  
  # Extract the model slope (assuming a simple linear model)
  slope <- coef(model)[2]
  
  # Compute the relative error
  relative_error <- prediction_error / slope
  
  return(relative_error)
}

# Compute relative errors for each model
relative_errors <- mapply(compute_relative_error, models, data_list, SIMPLIFY = FALSE)

# Print relative errors
relative_errors


##############################################################----
##################Visualisation##############################

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

# Define the function to extract model coefficients and details

extract_model_details <- function(model, data) {
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
  equation <- paste0("AGB = ", round(intercept, 2), " + ", round(slope, 2), " * ", names(coef(model))[2])
  details <- paste0("p-value = ", format.pval(p_value, digits = 2), "\nn = ", nrow(data))
  return(list(intercept = intercept, slope = slope, equation = equation, details = details))
}

# Define the function to create chm plots 
create_plotchm <- function(data, model_details, x_var, y_var, title_tag, color) {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.6, color = color) +  # Scatter plot points with custom color
    geom_abline(intercept = model_details$intercept, slope = model_details$slope, color = color) +  # Add robust regression line
    geom_smooth(method = "lmrob", se = TRUE, level = 0.95, color = color, fill = color, alpha = 0.2) +  # Add robust regression line with confidence interval
    theme_beautiful() +  # Use a classic theme with increased base font size
    labs(
      x = ifelse(x_var == "Mean_Canopy_Height(m)", "Mean Canopy Height (m)", x_var),
      y = y_var
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),  # Center the plot title and increase font size
      axis.title = element_text(size = 16),  # Increase axis title font size
      axis.text = element_text(size = 14),  # Increase axis text font size
      plot.tag = element_text(size = 18)  # Increase the tag font size
    ) +
    annotate("text", x = min(data[[x_var]]), y = max(data[[y_var]]) - 0.1 * max(data[[y_var]]),
             label = model_details$equation, hjust = 0, vjust = 1, size = 5, color = "black") +
    annotate("text", x = min(data[[x_var]]), y = max(data[[y_var]]) - 0.25 * max(data[[y_var]]),
             label = model_details$details, hjust = 0, vjust = 1, size = 5, color = "black") +
    labs(tag = title_tag) +
    ylim(0, 1000)  # Set y-axis limits
}

# Define the function to create ndvi plots 
# Define the function to create ndvi plots 
create_plotndvi <- function(data, model_details, x_var, y_var, title_tag, color) {
  ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.6, color = color) +  # Scatter plot points with custom color
    geom_smooth(method = "lmrob", se = TRUE, level = 0.95, color = color, fill = color, alpha = 0.2) +  # Add robust regression line with confidence interval
    theme_beautiful() +  # Use a classic theme with increased base font size
    labs(
      x = ifelse(x_var == "Mean_Canopy_Height(m)", "Mean Canopy Height (m)", x_var),
      y = y_var
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),  # Center the plot title and increase font size
      axis.title = element_text(size = 16),  # Increase axis title font size
      axis.text = element_text(size = 14),  # Increase axis text font size
      plot.tag = element_text(size = 18)  # Increase the tag font size
    ) +
    annotate("text", x = min(data[[x_var]]), y = max(data[[y_var]]) - 0.1 * max(data[[y_var]]),
             label = model_details$equation, hjust = 0, vjust = 1, size = 5, color = "black") +
    annotate("text", x = min(data[[x_var]]), y = max(data[[y_var]]) - 0.25 * max(data[[y_var]]),
             label = model_details$details, hjust = 0, vjust = 1, size = 5, color = "black") +
    labs(tag = title_tag) +
    ylim(0, 1000)  # Set y-axis limits
}

# # Create models
# rlowNDVI <- robustbase::lmrob(`AGB (g/m2)` ~ NDVI, data = Low)
# rlowCHM <- robustbase::lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data = Low)
# 
# ravNDVI <- robustbase::lmrob(`AGB (g/m2)` ~ NDVI, data = Average)
# ravCHM <- robustbase::lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data = Average)
# 
# rhighNDVI <- robustbase::lmrob(`AGB (g/m2)` ~ NDVI, data = High)
# rhighCHM <- robustbase::lmrob(`AGB (g/m2)` ~ `Mean_Canopy_Height(m)`, data = High)

# Extract model details
model_details_list <- list(
  rlowNDVI = extract_model_details(rlowNDVI, Low),
  rlowCHM = extract_model_details(rlowCHM, Low),
  ravNDVI = extract_model_details(ravNDVI, Average),
  ravCHM = extract_model_details(ravCHM, Average),
  rhighNDVI = extract_model_details(rhighNDVI, High),
  rhighCHM = extract_model_details(rhighCHM, High)
)

# Define custom colors
custom_colors <- c("Low" = "#4D3300", "Average" = "#808000", "High" = "#00994D")

# Create plots with custom colors
plotrlowNDVI <- create_plotndvi(Low, model_details_list$rlowNDVI, "NDVI", "AGB (g/m2)", "a)", custom_colors["Low"])
plotrlowCHM <- create_plotchm(Low, model_details_list$rlowCHM, "Mean_Canopy_Height(m)", "AGB (g/m2)", "b)", custom_colors["Low"])

plotravNDVI <- create_plotndvi(Average, model_details_list$ravNDVI, "NDVI", "AGB (g/m2)", "c)", custom_colors["Average"])
plotravCHM <- create_plotchm(Average, model_details_list$ravCHM, "Mean_Canopy_Height(m)", "AGB (g/m2)", "d)", custom_colors["Average"])

plotrhighNDVI <- create_plotndvi(High, model_details_list$rhighNDVI, "NDVI", "AGB (g/m2)", "e)", custom_colors["High"])
plotrhighCHM <- create_plotchm(High, model_details_list$rhighCHM, "Mean_Canopy_Height(m)", "AGB (g/m2)", "f)", custom_colors["High"])

# Print plots (or save them as needed)
print(plotrlowNDVI)
print(plotrlowCHM)
print(plotravNDVI)
print(plotravCHM)
print(plotrhighNDVI)
print(plotrhighCHM)


# Combine plots for side-by-side visualization
combined_plot <- (plotrlowCHM + plotravCHM + plotrhighCHM) / (plotrlowNDVI + plotravNDVI + plotrhighNDVI) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = 'a')

# Display combined plot
print(combined_plot)

# Save the combined plot using ggsave

ggsave("combined.jpg", height = 8, width = 19)

