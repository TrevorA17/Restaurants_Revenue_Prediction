# Load necessary libraries
library(tidyverse)

# Load dataset
restaurant_data <- read.csv("path/to/your/Restaurant_revenue.csv", colClasses = c(
  Number_of_Customers = "integer",
  Menu_Price = "numeric",
  Marketing_Spend = "numeric",
  Cuisine_Type = "factor",
  Average_Customer_Spending = "numeric",
  Promotions = "integer",
  Reviews = "integer",
  Monthly_Revenue = "numeric"
))

# Display the structure of the dataset
str(restaurant_data)

# View the first few rows of the dataset
head(restaurant_data)

# View the dataset in a separate viewer window
View(restaurant_data)
