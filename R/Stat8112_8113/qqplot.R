

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.6.2
par(mfrow=c(2,2))
t.3 <- rt(1000, 3)
t.5 <- rt(1000, 5)
t.10 <- rt(1000, 10)
t.100 <- rt(1000, 100)

qqnorm(t.3)
qqline(t.3,col='red', lwd=2)


qqnorm(t.5)
qqline(t.5,col='red', lwd=2)


qqnorm(t.10)
qqline(t.10,col='red', lwd=2)


qqnorm(t.100)
qqline(t.100,col='red', lwd=2)


