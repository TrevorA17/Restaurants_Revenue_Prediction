# Load necessary libraries
library(tidyverse)

# Load dataset
restaurant_data <- read.csv("data/Restaurant_revenue.csv", colClasses = c(
  Number_of_Customers = "integer",
  Menu_Price = "numeric",
  Marketing_Spend = "numeric",
  Cuisine_Type = "factor",
  Average_Customer_Spending = "numeric",
  Promotions = "factor",
  Reviews = "numeric",
  Monthly_Revenue = "numeric"
))

# Display the structure of the dataset
str(restaurant_data)

# Load necessary libraries
library(tidyverse)
library(psych)  # for descriptive statistics
library(GGally)  # for correlation plots

# Measures of Frequency
# Frequency of factor variables
frequency_promotions <- table(restaurant_data$Promotions)
frequency_cuisine <- table(restaurant_data$Cuisine_Type)
print("Frequency of Promotions:")
print(frequency_promotions)
print("Frequency of Cuisine Type:")
print(frequency_cuisine)

# Measures of Central Tendency
# Mean, Median, Mode
mean_revenue <- mean(restaurant_data$Monthly_Revenue)
median_revenue <- median(restaurant_data$Monthly_Revenue)
mode_revenue <- as.numeric(names(sort(table(restaurant_data$Monthly_Revenue), decreasing = TRUE)[1]))

print(paste("Mean Monthly Revenue:", mean_revenue))
print(paste("Median Monthly Revenue:", median_revenue))
print(paste("Mode Monthly Revenue:", mode_revenue))

# Measures of Distribution
# Range, Variance, Standard Deviation, Skewness, Kurtosis
range_revenue <- range(restaurant_data$Monthly_Revenue)
variance_revenue <- var(restaurant_data$Monthly_Revenue)
sd_revenue <- sd(restaurant_data$Monthly_Revenue)
skewness_revenue <- skew(restaurant_data$Monthly_Revenue)
kurtosis_revenue <- kurtosi(restaurant_data$Monthly_Revenue)

print(paste("Range of Monthly Revenue:", paste(range_revenue, collapse = " - ")))
print(paste("Variance of Monthly Revenue:", variance_revenue))
print(paste("Standard Deviation of Monthly Revenue:", sd_revenue))
print(paste("Skewness of Monthly Revenue:", skewness_revenue))
print(paste("Kurtosis of Monthly Revenue:", kurtosis_revenue))

# Measures of Relationship
# Correlation matrix
correlation_matrix <- cor(restaurant_data %>% select_if(is.numeric))

print("Correlation Matrix:")
print(correlation_matrix)

# Visualize the correlation matrix
ggcorr(restaurant_data %>% select_if(is.numeric), label = TRUE)





