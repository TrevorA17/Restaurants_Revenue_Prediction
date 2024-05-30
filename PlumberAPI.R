# Load the saved Linear Regression (lm) model
loaded_lm_model <- readRDS("./models/saved_lm_model.rds")

#* @apiTitle Restaurant Revenue Prediction Model API
#* @apiDescription Used to predict monthly restaurant revenue.

#* @param Number_of_Customers Number of customers
#* @param Menu_Price Menu price
#* @param Marketing_Spend Marketing spend
#* @param Cuisine_Type Cuisine type (e.g., Japanese)
#* @param Average_Customer_Spending Average customer spending
#* @param Promotions Promotions indicator (0 or 1)
#* @param Reviews Number of reviews

#* @get /predict_monthly_revenue

predict_monthly_revenue <- function(Number_of_Customers, Menu_Price, Marketing_Spend, 
                                    Cuisine_Type, Average_Customer_Spending, Promotions, Reviews) {
  
  # Create a data frame using the arguments
  to_be_predicted <- data.frame(
    Number_of_Customers = as.integer(Number_of_Customers),
    Menu_Price = as.numeric(Menu_Price),
    Marketing_Spend = as.numeric(Marketing_Spend),
    Cuisine_Type = as.factor(Cuisine_Type),
    Average_Customer_Spending = as.numeric(Average_Customer_Spending),
    Promotions = as.factor(Promotions),
    Reviews = as.integer(Reviews)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_lm_model, newdata = to_be_predicted)
  
  # Return the prediction
  return(prediction)
}
