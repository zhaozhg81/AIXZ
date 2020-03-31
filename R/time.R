library(forecast)
library(fpp)


local.constant <- function(X, h=1)
{
    n <- length(X)
    fitted <- array(0, n)
    forecast <- array(0, h)
    for(i in 1:n)
        fitted[i] <- mean( X[1:i] )
    for(i in 1:h)
        forecast[i] <- mean(X[1:n])
    list( fitted=fitted, residuals=X-fitted, forecast=forecast)
}


index <- read.table("./data/dji.txt")
lc <- local.constant(index$V1)
plot.ts( index$V1 )
points( c(1:length(index$V1)), lc$fitted, col='green', 'l')

data(oil)
oildata <- oil
plot(oildata, ylab="Oil (millions of tonnes)",xlab="Year")


## Oil

fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oildata, h=3)
plot(fit1, ylab="Oil (millions of tonnes)",   xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
  c("data", expression(alpha == 0.2), expression(alpha == 0.6),
    expression(alpha == 0.9999)),pch=1)

plot.forecast( fit3 )
acf(fit3$residuals, lag.max=20)
Box.test(fit3$residuals, lag=20, type="Ljung-Box")
ks.test( ( fit3$residuals -mean(fit3$residuals) )/sqrt( var(fit3$residuals)), 'pnorm' )


## The file http://robjhyndman.com/tsdldata/hurst/precip1.dat contains total annual rainfall in inches for
## London, from 1813-1912 (original data from Hipel and McLeod, 1994).
rain <- scan("./data/precip1.dat",skip=1)
rainseries <- ts(rain,start=c(1813))
plot.ts(rainseries)
rainseriesforecasts <- ses(rainseries, alpha=0.01, h=3)
sum( rainseriesforecasts$residuals^2 )
plot( forecast(rainseriesforecasts) )
Box.test(rainseriesforecasts$residuals, lag=20, type="Ljung-Box")


## This is a time series of the annual diameter of women’s skirts at the hem, from 1866 to 1911. The data is available in
## the file http://robjhyndman.com/tsdldata/roberts/skirts.dat (original data from Hipel and McLeod, 1994).

skirts <- scan("./data/skirts.dat",skip=5)
skirtsseries <- ts(skirts,start=c(1866))
plot.ts(skirtsseries)
f.skirts <- ses(skirtsseries, alpha=0.95, h=20)
plot( forecast(f.skirts) )
acf( f.skirts$residuals )



## Yield difference in Neitherlands
yield <- read.table("./data/yield.txt")
plot.ts(yield)
pacf( yield )
ar.yield <- arima( yield$V1, order=c(1,0,0 ) )
summary( ar.yield )
acf( ar.yield$residuals )
Box.test( ar.yield$residuals, lag=20, type="Ljung-Box" )

ar.forecast.yield <- forecast.Arima( ar.yield, h=20 )
plot.forecast( ar.forecast.yield )


## Difference of Elnino data: monthly El Nino data from 1955 to 1992.
## El Nino effect is thought to be a driver of world-wide weather.
## For this data set, we should use the MA model, not AR model.
library(HH)
data(elnino)
plot.ts(elnino)
d.elnino <- diff(elnino)
pacf( d.elnino )
acf( d.elnino )


ma.d.elnino <- arima( d.elnino, order=c(0,0,1) )
summary( ma.d.elnino )

acf( ma.d.elnino$residuals )
Box.test( ma.d.elnino$residuals, lag=20, type="Ljung-Box" )
ma.forecast.elnino <- forecast.Arima( ma.d.elnino, h=2 )
plot.forecast( ma.forecast.elnino )


## Iowa Nonfarm-Income
iowa <- read.table("./data/iowa_nonfarm_income.txt")$V1
rate <- diff(iowa)/iowa[1: (length(iowa)-1)]*100

plot.ts(rate)
arma.iowa <- Arima( rate, order=c(1,0,1) )

d.rate <- diff(rate)
plot.ts( d.rate )
arma.diff.iowa <- Arima( d.rate, order=c(0,0,1) )
arima.iowa <- Arima( rate, order=c(0,1,1) )
iowa.forecast <- forecast.Arima( arima.iowa, h=2 )
plot.forecast( iowa.forecast )

acf( iowa.forecast$residuals )
Box.test( iowa.forecast$residuals, lag=20, type="Ljung-Box" )
ks.test( iowa.forecast$residuals/sqrt(var(iowa.forecast$residuals)),'pnorm')


## Gas usage in Iowa State
gas <- read.table("./data/gas_iowa.txt")$V1
gas <- ts(gas, start=1971, freq=12 )

house <- read.table("./data/housing_starts.txt")$V1
plot.ts( gas )

acf( gas )
Season.diff <- diff( gas, lag=12 )
acf( Season.diff )

## We decide to use (0,0,1) (0,1,0)_S
season.arima <- Arima(gas, order=c(0,0,1), seasonal=list(order=c(0,1,0)) )
acf( season.arima$residuals )
Box.test( season.arima$residuals, lag=20, type="Ljung-Box" )



## Housing Start Price
house <- read.table("./data/housing_starts.txt")$V1
house <- ts(house, start=1965, freq=12 )

plot.ts( house )

acf( house )
Season.diff <- diff( house, lag=12 )
acf( Season.diff )

Season.diff.diff <- diff( Season.diff )
acf( Season.diff.diff )


## We decide to use (1,1,0) (1,1,0)_S
season.arima <- Arima(house, order=c(0,1,1), seasonal=list(order=c(0,1,1)) )
acf( season.arima$residuals )
Box.test( season.arima$residuals, lag=20, type="Ljung-Box" )
