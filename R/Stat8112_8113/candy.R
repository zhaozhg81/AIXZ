# %% [code]
## This code is contributed by Raquel Dourado from Kaggle

---
  title: "Candy Production : Time Series Forecast"
output: 
  html_document:
  toc: true
---
  
library('ggfortify')
library('data.table')
library('ggplot2')
library('forecast')
library('tseries')
library('urca')
library('dplyr')
library('TSstudio')
library("xts")

```

# 1 - Exploratory Data Analysis: Trend, Seasonality and Autocorrelation

```{r}
df<- read.csv('../input/candy_production.csv')
colnames(df)<-c("date", "production")
df$date<-as.Date(df$date, format="%Y-%m-%d")
CandyXTS<- xts(df[-1], df[[1]])
CandyTS<- ts(df$production, start=c(1972,1),end=c(2017,8), frequency=12 )
```


**Trend**
  ```{r}
ts_plot(CandyXTS, title="Candy Production Time Series")
ts_decompose(CandyTS)
```


**Seasonality**
  ```{r}
ts_heatmap(CandyTS)
ts_surface(CandyTS)
ts_seasonal(CandyTS, type="cycle")
ts_seasonal(CandyTS, type="box")
```
The plots suggest annual seasonality. We can observe higher production on october, november and december. 


**Autocorrelation**
  ```{r}
ggAcf(CandyTS)
```

The ACF plot above shows evidence of no-stationarity, with lines crossing the dashed lines (indicating correlation significantly different from zero).
Also, we already know this time series has some seasonality so, it has some time-dependence and is not stationary (there is autocorrelation)




# 2 - Simple models: Average, Naive, Seasonal Naive

```{r}
train<-window(CandyTS, start=c(1972,1), end=c(2009,12))
test<-window(CandyTS, start=c(2010,1))
```

**Average: forecast will be equal to the average of past data**
  ```{r}
m_mean<-meanf(train, h=92)
accuracy(m_mean, test)
```

**Naive: forecast will be equal to the last observation**
  ```{r}
m_naive<-naive(train, h=92)
accuracy(m_naive, test)
```

**Seasonal Naive: forecast will be equal to the last observation of same season**
  ```{r}
m_snaive<-snaive(train, h=92)
accuracy(m_snaive, test)
```

**Plots**
  
  ```{r}
autoplot(train)+
  autolayer(m_mean, series="Mean", PI=FALSE)+
  autolayer(m_naive, series="Naive", PI=FALSE)+
  autolayer(m_snaive, series="Seasonal Naive", PI=FALSE)+
  xlab('Year')+ylab('Candy Production')+
  ggtitle('Forecasts for Candy Production')+
  guides(colour=guide_legend(title='Forecast'))
```

Considering test set, the mean method showed better performance at some important metrics like RMSE and MAPE

# 3 - Exponential Smoothing Models: SES, Holt, Holt Winters, ETS

**Here, forecast will be based on weighted averages, with weights decreasing exponentially towards the past. **
  
  **Simple exponential: smoothing for level **
  
  ```{r}
m_ses<-ses(train, h=92)
accuracy(m_ses, test)
```
**Holt: smoothing for level and for trend**
  
  ```{r}
m_holt<-holt(train, h=92)
accuracy(m_holt, test)
```

**Holt Winters: smoothing for level, trend and seasonality**
  ```{r}
m_holtw<-hw(train, seasonal="additive", h=92)
accuracy(m_holtw, test)
```

**ETS: It allows other combinations of trend and seasonal components**  
  
  Error, Trend, Seasonality  
A= additive, M= multiplicative, N= none

```{r}
m_ets<-forecast(ets(train), h=92)
summary(m_ets)
accuracy(m_ets, test)
```


```{r}
autoplot(train)+
  autolayer(m_ses, series="Simple Exponential", PI=FALSE)+
  autolayer(m_holt, series="Holt Method", PI=FALSE)+
  autolayer(m_holtw, series="Holt_Winters", PI=FALSE)+
  autolayer(m_ets, series="ETS", PI=FALSE)+
  xlab('Month/Year')+ylab('Candy Production')+
  ggtitle('Forecasts for Candy Production')+
  guides(colour=guide_legend(title='Forecast'))
```

Considering test set, Holt Winters showed better RMSE and MAPE.

# 4 - ARIMA
ARIMA models combine  differencing, autoregression model and moving average model.
While the autoregressison model uses a linear combination of past values (regression), moving average model uses past forecasts errors in something close to a regression model.

auto.arima will search for the best ARIMA model, but it is a good practice also use a not automated method (I will do this later).

```{r}
autoarima <- auto.arima(train, allowdrift=F)
autoarima
```
```{r}
arima=Arima(train, order=c(4,1,1),
            seasonal=list(order=c(2,1,2), period=12))
arima
```
```{r}
arima_f =forecast(arima, h=92)
checkresiduals(arima)
```

Autocorrelations within the threshold limit and a large p-value at Ljung-Box test suggest that the residuals are white noise. Good.

```{r}
accuracy(arima_f, test)
```

```{r}
autoplot(train)+
  autolayer(arima_f, series="ARIMA", PI=FALSE)+
  xlab('Month/Year')+ylab('Candy Production')+
  ggtitle('Forecasts for Candy Production')+
  guides(colour=guide_legend(title='Forecast'))
```

After ARIMA, we still have better RMSE and MAPE with Holt Winters method. 
For future analysis, may be a good option to work with a reduced time series and more advanced methods.

Reference: Forecasting: Principles and Practice by Rob J Hyndman and George Athanasopoulos