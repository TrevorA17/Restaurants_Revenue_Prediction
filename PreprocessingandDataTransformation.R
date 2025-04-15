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

# Round off specified columns to 2 decimal places
restaurant_data <- restaurant_data %>%
  mutate(
    Monthly_Revenue = round(Monthly_Revenue, 2),
    Average_Customer_Spending = round(Average_Customer_Spending, 2),
    Marketing_Spend = round(Marketing_Spend, 2),
    Menu_Price = round(Menu_Price, 2)
  )


# Display the first few rows of the dataset to verify the rounding
head(restaurant_data)

# Display the structure of the dataset to verify the data type
str(restaurant_data)

# View the dataset in a separate viewer window
View(restaurant_data)
