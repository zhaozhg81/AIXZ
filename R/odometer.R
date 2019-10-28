car <- read.csv("data/car_Odometer.csv")

lm.car <- lm( Price~Odometer, data=car )

summary( lm.car )

anova( lm.car )

newdata <- data.frame(Odometer=c(10,36,50 ) )

predict(lm.car, newdata, interval="confidence", level=0.95 )


predict(lm.car, newdata, interval="predict", level=0.95 )
