figdir="~/Dropbox/Apps/Overleaf/Stat_for_Busi_Research/figure/"

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
lc.oil <- local.constant(oildata)

plot(c(1:length(oil)), oildata, ylab="Oil (millions of tonnes)",xlab="Year",'l')
points( c(1:length(oil)), lc.oil$fitted, col='green', 'l')


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

plot( forecast( fit3 ) )
acf(fit2$residuals, lag.max=20)
acf(fit3$residuals, lag.max=20)

Box.test(fit2$residuals, lag=20, type="Ljung-Box")
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
plot( acf( rainseriesforecasts$residuals ) )

plot( rain, type="o" )
plot( fitted( rainseriesforecasts), col='green', type='o')


## Yield in Neitherlands
yield <- read.table("./data/yield.txt")
plot.ts(yield)
acf( yield ) 
pacf( yield )
ar.yield <- arima( yield$V1, order=c(1,0,0 ) )
ar.yield
acf( ar.yield$residuals )

Box.test( ar.yield$residuals, lag=20, type="Ljung-Box" )

ar.forecast.yield <- forecast( ar.yield, h=20 )
plot( ar.forecast.yield ) 

ma.yield <- arima( yield$V1, order=c(0,0,1) )
acf( ma.yield$residuals )

## Simulated data from MA(1)
T <- 1000
theta=0.5
z.ma <- array(0, T)
z.ma[1] <- rnorm(1)
eps.prev <- z.ma[1]
for(t in 2:T)
  {
    eps <- rnorm(1)
    z.ma[t] <- eps + 0.8 * eps.prev
    eps.prev <- eps
  }

pacf( z.ma )

acf( z.ma )

postscript(paste(figdir, "pacf_MA_pos.eps",sep=""), horizontal=FALSE)
pacf(z.ma)
dev.off()

postscript(paste(figdir, "acf_MA_pos.eps",sep=""), horizontal=FALSE)
acf(z.ma)
dev.off()


## AR(1)

T <- 1000
theta=0.5
z.ma <- array(0, T)
z.ma[1] <- rnorm(1)
for(t in 2:T)
{
  z.ma[t] <- 0.8*z.ma[t-1] + rnorm(1)
}

pacf( z.ma )

acf( z.ma )

## MA(2), negative
T <- 1000
theta=0.5
z.ma <- array(0, T)
z.ma[1] <- rnorm(1)
eps.prev <- z.ma[1]
eps.prev.two <- rnorm(1)
for(t in 3:T)
{
  eps <- rnorm(1)
  z.ma[t] <- eps - 0.8 * eps.prev + 0.5 * eps.prev.two
  eps.prev.two <- eps.prev
  eps.prev <- eps
}

pacf( z.ma )

acf( z.ma )

postscript(paste(figdir, "pacf_MA_2.eps",sep=""), horizontal=FALSE)
pacf(z.ma)
dev.off()

postscript(paste(figdir, "acf_MA_2.eps",sep=""), horizontal=FALSE)
acf(z.ma)
dev.off()


## Difference of Elnino data: monthly El Nino data from 1955 to 1992.
## El Nino effect is thought to be a driver of world-wide weather.
## For this data set, we should use the MA model, not AR model.
library(HH)
data(elnino)
plot.ts(elnino)
acf( elnino )
pacf( elnino )

d.elnino <- diff(elnino)
pacf( d.elnino )
acf( d.elnino ) 

ma.d.elnino <- arima( d.elnino, order=c(0,0,1) )
summary( ma.d.elnino )

acf( ma.d.elnino$residuals ) 
Box.test( ma.d.elnino$residuals, lag=20, type="Ljung-Box" )
ma.forecast.elnino <- forecast( ma.d.elnino, h=20 )
plot( ma.forecast.elnino )

elnino.arima = arima( elnino, order=c(0,1,1) )
acf( elnino.arima$residuals)


## Iowa Nonfarm-Income
iowa <- read.table("./data/iowa_nonfarm_income.txt")$V1
rate <- diff(iowa)/iowa[1: (length(iowa)-1)]*100
plot.ts(rate)

d.rate <- diff(rate)
plot.ts( d.rate )

arma.diff.iowa <- arima( d.rate, order=c(0,0,1) )
acf( arma.diff.iowa$residuals )
summary( arma.diff.iowa )

## Using ARIMA model with difference
arima.iowa <- arima( rate, order=c(0,1,1) )
iowa.forecast <- forecast( arima.iowa, h=20 )
summary( arima.iowa )
plot( iowa.forecast )

arma.diff.iowa <- arima( d.rate, order=c(0,0,1), include.mean=0 )
acf( arma.diff.iowa$residuals )
summary( arma.diff.iowa )


####
## Model with seasonal trend

## Gas usage in Iowa State
gas <- read.table("./data/gas_iowa.txt")$V1
gas <- ts(gas, start=1971, freq=12 )
plot.ts( gas )

acf( gas, lag=50 )
Season.diff <- diff( gas, lag=12 )
acf( Season.diff, lag=50 )


## We decide to use (0,0,1) (0,1,0)_S
gas.arima <- arima(gas, order=c(0,0,1), seasonal=list(order=c(0,1,0)) )
acf( gas.arima$residuals )
Box.test( gas.arima$residuals, lag=20, type="Ljung-Box" )

## We decide to use (0,0,1) (0,1,1)
gas.arima.2 <- arima(gas, order=c(0,0,1), seasonal=list(order=c(0,1,1)) )
Box.test( gas.arima.2$residuals, lag=20, type="Ljung-Box" )
acf( gas.arima.2$residuals )
summary( gas.arima.2)

plot( forecast( gas.arima.2, h=20 ))

gas.arima.3 <- arima(gas, order=c(1,0,0), seasonal=list(order=c(2,1,0)) )
acf( gas.arima.3$residuals )
summary( gas.arima.3 )

gas.arima.4 <- arima(gas, order=c(0,0,1), seasonal=list(order=c(2,1,0)) )
acf( gas.arima.4$residuals )


## Housing Start Price
house <- read.table("./data/housing_starts.txt")$V1
house <- ts(house, start=1965, freq=12 )

plot.ts( house )

acf( house, lag=48 )
Season.diff <- diff( house, lag=12 )

acf( Season.diff, lag=48 )
pacf( Season.diff, lag=48 )



house.arima <- arima(house, order=c(1,0,0), seasonal=list(order=c(0,1,1)) )
acf( house.arima$residuals )
Box.test( house.arima$residuals, lag=20, type="Ljung-Box" )
plot( forecast( house.arima, h=25 ) )
summary( house.arima )


house.arima.2 <- arima( house, order=c(2,0,0), seasonal=list( order=c(0,1,1) ))
acf( house.arima.2$residuals)
Box.test( house.arima.2$residuals, lag=25, type="Ljung-Box" )
summary( house.arima.2 )