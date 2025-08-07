############LIBRARIES################################----
library(spdep)

library(tidyverse)
library(mlr3)
library(mlr3verse)
library(mlr3learners)
library(GGally)        # works with mlr3viz
library(Boruta)        # Feature selection
library(mlr3spatiotempcv)  ### has resampling methods to account for spatiotemporal autocorrelation
library(sf)
library(sf)
library(tmap)
library(ggtext)

########################THEME BEAUTIFUL FOR PLOTS#####----
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


###########LOADING DATA################################----

# Load data
forage_data = read_csv("data/Data.csv")
view(forage_data)

################VISUALISE DATA###########################----

forage_data |> 
  ggplot(aes(x = as.factor(AOI), y = grazing_value)) +
  geom_boxplot() +
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.5) +  # Violin plot
  geom_jitter(width = 0.2, alpha = 0.7, color = "black") +     # Jittered points
  labs(x = "AOI", y = "Synthetic Grazing Value") +
  theme_beautiful()

####################

# Ensure valid column names
colnames(forage_data) = make.names(colnames(forage_data), unique = TRUE)

# Subset the data to only include the columns grazing_value,
## Mean_Canopy_Height.m., NDVI, EVI, OSAVI, TDVI, GDVI,B1,B2,B3,B4,B5, TCARI, MCARI, NDRE, GCI

forage_data_subset = forage_data |>  
  select(AOI, Northing_m, Easting_m, grazing_value, Mean_Canopy_Height_m, NDVI, EVI, OSAVI, TDVI, GDVI,B1,B2,B3,B4,B5, TCARI, MCARI, NDRE, GCI)


####################### APPLY BORUTA FEATURE SELECTION #######################----

set.seed(123)  # Ensure reproducibility
boruta_result = Boruta(grazing_value ~ ., data = forage_data_subset, doTrace = 2)

# Print results of Boruta feature selection
print(boruta_result)

# Confirm important features
final_features = getSelectedAttributes(boruta_result, withTentative = TRUE)
cat("Selected Features:", final_features, "\n")

# Subset dataset with selected features
forage_data_selected = forage_data|> select(all_of(c("grazing_value", final_features)))

str(forage_data_selected)
#




########################TASK#################################----
# Setting up regression task

task_forage = TaskRegrST$new(
  id = "forage_quality",
  backend = forage_data_selected,
  target = "grazing_value",
  coordinate_names = c("Easting_m", "Northing_m"),
  crs = 32734,
  coords_as_features = FALSE
)

task_forage$set_col_roles("AOI", "group")

?mlr3spatiotempcv::TaskRegrST
####View task summary
task_forage

##### Plot task to get graphical summary of the distribution of the target and 
##feature values
autoplot(task_forage, type = "pairs")

###################LEARNERS#################################----

# Define the random forest learner
learner_rf = lrn("regr.ranger", num.trees = 500, importance = "impurity", predict_type = "se")
learner_rf

#################TRAINING###################################----

####Model is trained by passing a task to a learner with the $train() method:
set.seed(123)  # Set a fixed random seed
splits = partition(task_forage)  # Randomly split the data

splits
#####Train the model
set.seed(123)  # Fix randomness
learner_rf$train(task_forage, row_ids = splits$train)      #Train model

#####access hyperparameters
learner_rf$param_set

#########################PREDICTING#######################----

# Make predictions
predictions = learner_rf$predict(task_forage, row_ids = splits$test)

# Print first few predictions
print(predictions)

####Comparing predicted and ground truth values
predictions = learner_rf$predict(task_forage, splits$test)
autoplot(predictions)

# Compute performance metrics
rsq_value <- predictions$score(msr("regr.rsq"))  # R-squared

rmse_value <- predictions$score(msr("regr.rmse"))  # RMSE
mae_value <- predictions$score(msr("regr.mae"))  # MAE

# Print results
cat("R-squared:", rsq_value, "\n")
cat("RMSE:", rmse_value, "\n")
cat("MAE:", mae_value, "\n")


# Perform cross-validation
set.seed(123)  # Ensures reproducibility
 
resampling <- rsmp("custom_cv")
resampling$instantiate(task_forage, col = "AOI")

autoplot(resampling, task_forage)

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
  theme_beautiful()
##########################################################----

###
##
###
#
