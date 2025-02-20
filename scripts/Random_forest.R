############LIBRARIES################################----

library(tidyverse)
library(mlr3verse)
library(GGally)        # works with mlr3viz

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
forage_data <- read_csv("data/Data.csv")
view(forage_data)

################VISUALISE DATA###########################----

forage_data %>%
  ggplot(aes(x = as.factor(AOI), y = grazing_value)) +
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.5) +  # Violin plot
  geom_jitter(width = 0.2, alpha = 0.7, color = "black") +     # Jittered points
  labs(x = "AOI", y = "Synthetic Grazing Value") +
  theme_beautiful()


# Ensure valid column names
colnames(forage_data) <- make.names(colnames(forage_data), unique = TRUE)

# Subset the data to only include the columns grazing_value,
## Mean_Canopy_Height.m., NDVI, EVI, OSAVI, TDVI, GDVI,B1,B2,B3,B4,B5

forage_data_subset = forage_data %>% 
  select(grazing_value, Mean_Canopy_Height_m, NDVI, EVI, OSAVI, TDVI, GDVI,B1,B2,B3,B4,B5)

str(forage_data_subset)


########################TASK#################################----
# Setting up regression task
task_forage = TaskRegr$new(
  id = "forage_quality",
  backend = forage_data_subset,
  target = "grazing_value"
)

####View task summary
task_forage

##### Plot task to get graphical summary of the distribution of the target and 
##feature values
autoplot(task_forage, type = "pairs")

#####retrieving task metadata
c(task_forage$nrow, task_forage$ncol)  #### the dimensions of the task


c(Features = task_forage$feature_names,     #### number of features and target
  Target = task_forage$target_names)        ### column

head(task_forage$row_ids)                    ### check row IDs


task_forage$data()                          # retrieve all data


task_forage$data(rows = c(1, 5, 89), cols = task_forage$feature_names) # retrieve 
#data for rows with IDs 1, 5, and 10 and all feature columns



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

###we will tell the model to only use the training data by passing the row IDs 
##from partition to the row_ids argument of $train()

learner_rf$train(task_forage, row_ids = splits$train)      #Train model

#####access hyperparameters
learner_rf$param_set


#########################PREDICTING#######################----

## we call the $predict() method of our trained learner and again will use the 
##row_ids argument, but this time to pass the IDs of our test set


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

# Define resampling strategy (5-fold cross-validation)
resampling <- rsmp("cv", folds = 5)

print(task_forage)

# Perform cross-validation
set.seed(123)  # Ensures reproducibility
resampling <- rsmp("cv", folds = 5)  # Define 5-fold CV

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



