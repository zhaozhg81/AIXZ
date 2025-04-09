# Load necessary libraries
library(forecast)
library(ggplot2)


data(oil)
oildata <- oil
lc.oil <- local.constant(oildata)

# Read the data from CSV file
## data <- read.csv("../data/sample_data.csv")


# Set the Date column as the index
data_ts <- ts(oil)

data_ts = oil

fit_holt <- holt( data_ts, damped =FALSE) 
fit_damped <- holt(data_ts,  damped = TRUE)


# Holt Model
fit_holt <- HoltWinters(data_ts)
plot(data_ts)
lines( fit_holt$fitted[,1], col='green', type='o')

# Holt Model with Damped Trend
fit_damped <- HoltWinters(data_ts, beta = FALSE, gamma = FALSE, damped = TRUE)

fit_damped <- holt(data_ts,  damped = TRUE)

# Holt-Winters Additive Model
fit_hw_additive <- HoltWinters(data_ts, seasonal = "additive")

lines( fit_hw_additive$fitted[,1], col='red', type='o')

# Holt-Winters Multiplicative Model
fit_hw_multiplicative <- HoltWinters(data_ts, seasonal = "multiplicative")
lines( fit_hw_multiplicative$fitted[,1], col='green', type='o')

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
