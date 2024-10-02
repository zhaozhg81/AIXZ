tread=read.table("data/3502/treadwear.txt",header=T)

head(tread)

tread.lm = lm(groove~mileage, data=tread)
plot( tread$mileage, tread$groove)
abline(tread.lm, col='red')

summary( tread.lm) 

plot(tread.lm$fitted.values, tread.lm$residuals)
abline(0,0,col='red')