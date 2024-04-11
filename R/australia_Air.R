library(fpp)

data(ausair)
air <- window(ausair, start=1990)


fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
fc3 <- holt( air, damped=TRUE, h= 15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  autolayer(fc3, series="Damped Holt's method, auto choose phi", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))


data("austourists")
aust <- window(austourists,start=2005)
plot.ts(aust)

fc.aust <- holt( aust, h=1)
fc.pred <- window( fc.aust$fitted, start=2005)

fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
fit3 <- hw(aust,seasonal="additive", damped=TRUE)

plot.ts(aust)
points( 2005+c(0:23)/4, fit1$fitted, 'l', col='red' )
points( 2005+c(0:23)/4, fit2$fitted, 'l', col='green' )
points( 2005+c(0:23)/4, fit3$fitted, 'l', col='blue' )

autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  autolayer(fit3, series="HW additive forecasts, damped",
            PI=FALSE)+
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))


