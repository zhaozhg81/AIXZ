bloodpressure = read.table("data/3502/bloodpressure.txt", header=TRUE)


cor( cbind( bloodpressure$Age, bloodpressure$Weight, bloodpressure$BSA, bloodpressure$Dur, bloodpressure$Pulse, bloodpressure$Stress) )
pairs(~Age+Weight+BSA+Dur+Pulse+Stress, data=bloodpressure)

cor.test(bloodpressure$Weight, bloodpressure$BSA)

## Model with x3 only
lm.x3= lm(BP~BSA, data=bloodpressure)
lm.x23 = lm(BP~Weight +BSA, data=bloodpressure)

summary(lm.x3)
summary(lm.x23)

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

