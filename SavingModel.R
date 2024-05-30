# Saving the Linear Regression (lm) model
saveRDS(lm_model, "./models/saved_lm_model.rds")

# Load the saved model
loaded_lm_model <- readRDS("./models/saved_lm_model.rds")

# Prepare new data for prediction
new_data <- data.frame(
  Number_of_Customers = 61,
  Menu_Price = 43.11763549,
  Marketing_Spend = 12.66379252,
  Cuisine_Type = "Japanese",
  Average_Customer_Spending = 36.23613252,
  Promotions = "0",
  Reviews = 45
)

# Use the loaded model to make predictions
predictions_loaded_model <- predict(loaded_lm_model, newdata = new_data)

# Print predictions
print(predictions_loaded_model)
