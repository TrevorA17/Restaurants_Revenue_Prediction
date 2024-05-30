# Load necessary libraries
library(caTools)  # For data splitting

# Split the data into training and testing sets
set.seed(123)  # Set seed for reproducibility
split <- sample.split(restaurant_data$Monthly_Revenue, SplitRatio = 0.7)
train_data <- subset(restaurant_data, split == TRUE)
test_data <- subset(restaurant_data, split == FALSE)

# Summary of the split
cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Load necessary libraries
library(boot)  # For bootstrapping

# Define the function to compute the statistic of interest (e.g., mean)
bootstrap_mean <- function(data, indices) {
  sample_data <- data[indices, ]
  return(mean(sample_data$Monthly_Revenue))
}

# Perform bootstrap resampling
set.seed(123)  # Set seed for reproducibility
boot_results <- boot(restaurant_data, statistic = bootstrap_mean, R = 1000)

# Summary of bootstrap results
summary(boot_results)

# Load necessary libraries
library(caret)  # For cross-validation

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Train a linear regression model using cross-validation
set.seed(123)  # Set seed for reproducibility
lm_model <- train(Monthly_Revenue ~ ., data = train_data, method = "lm", trControl = ctrl)

# Summary of the cross-validated linear regression model
print(lm_model)

# Load necessary libraries
library(caret)       # For model training and evaluation
library(rpart)       # For decision trees
library(randomForest)# For random forest

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Train a linear regression model
set.seed(123)  # Set seed for reproducibility
lm_model <- train(Monthly_Revenue ~ ., data = train_data, method = "lm", trControl = ctrl)

# Train a decision tree model
set.seed(123)  # Set seed for reproducibility
dt_model <- train(Monthly_Revenue ~ ., data = train_data, method = "rpart", trControl = ctrl)

# Train a random forest model
set.seed(123)  # Set seed for reproducibility
rf_model <- train(Monthly_Revenue ~ ., data = train_data, method = "rf", trControl = ctrl)

# Summary of the models
print(lm_model)
print(dt_model)
print(rf_model)
