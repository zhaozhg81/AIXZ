bloodpressure = read.table("data/3502/bloodpressure.txt", header=TRUE)

## Model with Age only
lm.1 = lm( BP~Age, data= bloodpressure)

plot(bloodpressure$Age, bloodpressure$BP, pch=16, col='blue')
abline(lm.1, col='red', lwd=2)

plot(lm.1$fitted.values, lm.1$residuals, pch=16, col='blue')
abline(0,0,col='black', lwd=2)

plot(bloodpressure$Weight, lm.1$residuals, pch=16, col='blue')
abline(0, 0, col='black', lwd=2)

## Model with Age and Weight
lm.2 = lm(BP~Age + Weight, data=bloodpressure)

plot(lm.2$fitted.values, lm.2$residuals, pch=16, col='blue')
abline(0,0,col='black', lwd=2)

plot(bloodpressure$Dur, lm.2$residuals, pch=16, col='blue')
abline(0, 0, col='black', lwd=2)
