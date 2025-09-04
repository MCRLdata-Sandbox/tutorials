## This script creates a template for creating a more robust Random Forest than
## the previous tutorial and leverages the tidymodels package, which adds standard
## features and easy access to a much broader suite of modeling functions. is a base template from Chat for running a random forest model to do basic
## tasks: split data, clean it up, run the model, then plot correlations and 
## feature importance. Note that these data do not contain substantial information
## about each other, and therefore the model functions poorly. Data that are not
## randomly generated will likely do better! :)
##
## Peter Regier with help from gpt4.0
## 2025-07-19

# Load necessary packages. If you have installed these packages, pacman may have
## issues. try install.packages("your_package") to fix issues with packages not loading
require(pacman)
p_load(tidyverse, tidymodels, ranger, cowplot)

# Set seed for reproducibility
set.seed(123)

n_rows = 1000

# Create a dataset with numeric columns k and a1 through a20
df_test <- tibble(
  k = rnorm(n_rows), # Dependent variable
  a1 = rnorm(n_rows),
  a2 = rnorm(n_rows),
  a3 = rnorm(n_rows),
  a4 = rnorm(n_rows),
  a5 = rnorm(n_rows))

# Split the dataset into training and testing sets
set.seed(123)
data_split <- initial_split(df_test, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Create a recipe with step_normalize and step_corr
recipe <- recipe(k ~ ., data = train_data) %>%
  step_normalize(all_predictors()) %>% # Normalize predictors
  step_corr(all_predictors()) # Remove highly correlated predictors

# Prep recipe (this step processes the data)
prep_rec <- prep(recipe)

# Apply preprocessing to train and test data
train_data_processed <- bake(prep_rec, new_data = train_data)
test_data_processed <- bake(prep_rec, new_data = test_data)

# Define the model specification for random forest (using ranger)
rf_model <- rand_forest() %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

# Create workflow combining recipe and model
workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_model)

# Fit the model on the training data
rf_fit <- fit(workflow, data = train_data)

# Make predictions on the test data
predictions <- predict(rf_fit, new_data = test_data_processed) %>%
  bind_cols(test_data_processed) # Combine predictions with actual test data

# Plot predicted vs actual for k
p_cor <- ggplot(predictions, aes(x = k, y = .pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(title = "Predicted vs Actual",
       x = "Actual k",
       y = "Predicted k") +
  theme_minimal()

# Extract feature importance
importance <- pull_workflow_fit(rf_fit)$fit$variable.importance
importance_df <- tibble(
  predictor = names(importance),
  importance = importance
) %>%
  arrange(desc(importance))

# Plot feature importance
p_fi <- ggplot(importance_df, aes(x = reorder(predictor, importance), y = importance)) +
  geom_col() +
  coord_flip() +
  labs(title = "Feature Importance",
       x = "Predictors",
       y = "Importance") +
  theme_minimal()

plot_grid(p_cor, p_fi, nrow = 1)


###############################
###############################


## In order to make this simpler, let's turn this workflow into a function: 
tidy_rf <- function(data, var){
  # Dynamically capture the variable name passed as input
  var <- deparse(substitute(var))  # Turn var dynamically into a string if passed unquoted
  data <- data %>%
    rename("dep" = !!sym(var))       # Rename dynamically using the column name provided
  
  # Split the dataset into training and testing sets
  set.seed(123)
  data_split <- initial_split(data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Create a recipe with step_normalize and step_corr
  recipe <- recipe(dep ~ ., data = train_data) %>%
    step_normalize(all_numeric_predictors()) #%>% # Normalize numeric predictors
    #step_corr(all_numeric_predictors())         # Remove highly correlated predictors
  
  # Prep recipe to preprocess the data
  prep_rec <- prep(recipe)
  
  # Dynamically extract variables remaining after preprocessing
  remaining_vars <- juice(prep_rec) %>% names()
  
  # Ensure training and test data include only remaining variables
  train_data_clean <- train_data %>%
    select(any_of(remaining_vars))
  
  test_data_clean <- test_data %>%
    select(any_of(remaining_vars))
  
  # Apply preprocessing to the cleaned datasets
  train_data_processed <- bake(prep_rec, new_data = train_data_clean)
  test_data_processed <- bake(prep_rec, new_data = test_data_clean)
  
  # Define the model specification for random forest (using ranger)
  rf_model <- rand_forest() %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("regression")
  
  # Create workflow combining recipe and model
  workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(rf_model)
  
  # Fit the model on the processed training data
  rf_fit <- fit(workflow, data = train_data_processed)
  
  # Make predictions on the preprocessed test data
  predictions <- predict(rf_fit, new_data = test_data_processed) %>%
    bind_cols(test_data_processed) # Combine predictions with actual test data
  
  # Plot predicted vs actual for dep (target variable)
  p_cor <- ggplot(predictions, aes(x = dep, y = .pred)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = 'red') +
    labs(title = "Predicted vs Actual",
         x = paste("Actual", var),
         y = paste("Predicted", var)) +
    theme_minimal()
  
  # Extract feature importance
  importance <- pull_workflow_fit(rf_fit)$fit$variable.importance
  
  importance_df <- tibble(
    predictor = names(importance),
    importance = importance
  ) %>%
    arrange(desc(importance))
  
  # Plot feature importance
  p_fi <- ggplot(importance_df, aes(x = reorder(predictor, importance), y = importance)) +
    geom_col() +
    coord_flip() +
    labs(title = "Feature Importance",
         x = "Predictors",
         y = "Importance") +
    theme_minimal()
  
  plot_grid(p_cor, p_fi, nrow = 1)
}

tidy_rf(df_test, k)

