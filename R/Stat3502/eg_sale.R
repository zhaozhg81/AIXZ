## Let's say we are trying to model the daily sales of a retail 
## store based on the number of promotional emails sent to customers. 
## The sales data shows a strong upward trend over time (e.g., due 
## to seasonal effects), but we apply a simple linear regression model 
## where sales depend only on the number of emails sent out. 
## This model doesn't account for the time-dependent trend 
## (like seasonality or trends in customer behavior).

sales = read.csv("data/3502/sales.csv")
# Load necessary libraries
library(ggplot2)

# Fit a simple linear regression model
model <- lm(Sales ~ Promotional_Emails, data = sales)

plot(sales$Promotional_Emails, sales$Sales,pch=16, col='blue' )
abline(model, col='red', lwd=2)

plot(c(1:100), model$residuals, pch=16, col='blue')
abline(0,0, col='black', lwd=2)