# Load required libraries
library(tidyverse)
library(mlr3verse)
library(mlr3learners)  # Random forest learner
library(mlr3viz)       # Visualization
library(mlr3tuning)    # Hyperparameter tuning
library(mlr3measures)  # Performance metrics
library(mlr3pipelines) # For preprocessing (if needed)

# Load data
forage_data <- read_csv("data/Data.csv")

# Ensure valid column names
colnames(forage_data) <- make.names(colnames(forage_data), unique = TRUE)

# Select specific predictor variables (including the target)
forage_data_selected <- forage_data %>% 
  select(grazing_value, Mean_Canopy_Height.m., NDVI, EVI, OSAVI, TDVI, GDVI,B1,B2,B3,B4,B5)

# Create a regression task using only the selected predictors
task_forage = TaskRegr$new(
  id = "forage_quality",
  backend = forage_data_selected,
  target = "grazing_value"
)

# Define the random forest learner
learner_rf = lrn("regr.ranger", num.trees = 500, importance = "impurity")

# Train the model
learner_rf$train(task_forage)

# Make predictions
predictions <- learner_rf$predict(task_forage)

# Print first few predictions
print(predictions)
# Compute performance metrics
rsq_value <- predictions$score(msr("regr.rsq"))  # R-squared
rmse_value <- predictions$score(msr("regr.rmse"))  # RMSE
mae_value <- predictions$score(msr("regr.mae"))  # MAE

# Print results
cat("R-squared:", rsq_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")

# Define resampling strategy (5-fold cross-validation)
resampling <- rsmp("cv", folds = 5)

print(task_forage)

# Perform cross-validation
resampling <- rsmp("cv", folds = 5)
rr <- resample(task_forage, learner_rf, resampling)
cat("Cross-validated R-squared:", rr$aggregate(msr("regr.rsq")), "\n")
cat("Cross-validated RMSE:", rr$aggregate(msr("regr.rmse")), "\n")
cat("Cross-validated MAE:", rr$aggregate(msr("regr.mae")), "\n")

# Feature importance
importance_values <- learner_rf$importance()
print(importance_values)

# Convert importance values to a dataframe
importance_df <- data.frame(
  Feature = names(importance_values),
  Importance = importance_values
)

# Plot
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flip axes for readability
  labs(title = "Feature Importance (Random Forest)",
       x = "Predictors", y = "Importance Value") +
  theme_minimal()

ggplot(data.frame(Truth = predictions$truth, Predicted = predictions$response), aes(x = Truth, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  # 1:1 reference line
  labs(title = "Actual vs. Predicted Grazing Value",
       x = "Actual Grazing Value", y = "Predicted Grazing Value") +
  theme_minimal()

