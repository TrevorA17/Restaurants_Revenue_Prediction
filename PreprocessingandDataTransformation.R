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


# Confirm presence of missing values

# Summary of missing values for each column
missing_values_summary <- colSums(is.na(restaurant_data))
print("Summary of missing values for each column:")
print(missing_values_summary)

# Total number of missing values in the dataset
total_missing_values <- sum(is.na(restaurant_data))
print(paste("Total number of missing values in the dataset:", total_missing_values))
