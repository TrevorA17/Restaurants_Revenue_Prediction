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
