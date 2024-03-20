# Load necessary libraries
library(forecast)
library(ggplot2)

# Read the data from CSV file
data <- read.csv("path/to/your/sample_data.csv")

# Convert Date column to Date format
data$Date <- as.Date(data$Date)

# Set the Date column as the index
data_ts <- ts(data$Value, frequency = 365.25)

# Holt Model
fit_holt <- HoltWinters(data_ts)

# Holt Model with Damped Trend
fit_damped <- HoltWinters(data_ts, beta = FALSE, gamma = FALSE, damped = TRUE)

# Holt-Winters Additive Model
fit_hw_additive <- HoltWinters(data_ts, seasonal = "additive")

# Holt-Winters Multiplicative Model
fit_hw_multiplicative <- HoltWinters(data_ts, seasonal = "multiplicative")

# Forecasting
forecast_holt <- forecast(fit_holt, h = 12)
forecast_damped <- forecast(fit_damped, h = 12)
forecast_hw_additive <- forecast(fit_hw_additive, h = 12)
forecast_hw_multiplicative <- forecast(fit_hw_multiplicative, h = 12)

# Plotting
autoplot(data_ts) +
  autolayer(fitted(fit_holt), series = "Holt Forecast") +
  autolayer(fitted(fit_damped), series = "Holt Damped Forecast") +
  autolayer(fitted(fit_hw_additive), series = "HW Additive Forecast") +
  autolayer(fitted(fit_hw_multiplicative), series = "HW Multiplicative Forecast") +
  autolayer(forecast_holt, series = "Holt Forecast (Next 12 Months)") +
  autolayer(forecast_damped, series = "Holt Damped Forecast (Next 12 Months)") +
  autolayer(forecast_hw_additive, series = "HW Additive Forecast (Next 12 Months)") +
  autolayer(forecast_hw_multiplicative, series = "HW Multiplicative Forecast (Next 12 Months)") +
  ggtitle("Forecasting with Holt and Holt-Winters Models") +
  xlab("Date") +
  ylab("Value") +
  theme_bw() +
  theme(legend.position = "top")
