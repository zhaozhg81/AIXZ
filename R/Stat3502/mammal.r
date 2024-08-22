mydata=read.table("C:/Users/dongy/Desktop/linear regression/mammal.txt",header=T)

x=mydata$Birthwgt
y=mydata$Gestation

out=lm(y~x)

qqnorm(out$residuals,main="")
qqline(out$residuals)

shapiro.test(out$residuals)
# test for residual nomality
# null hypothesis: sample is from normal population 


yhat=out$fitted.values

yhat[order(yhat)]

res=out$residuals

new_res=res[order(yhat)]

plot(yhat,res,xlab="fitted response", ylab="residual")
abline(h=0,lty=2)
points(yhat[order(yhat)[1:5]],res[order(yhat)[1:5]],col="red")

bartlett.test(res[order(yhat)],g=c(rep(1,5),rep(2,6)))

# test for equal variance
# null hypothesis: all variances are equal

# p-value is 0.05199, marginally significant
# equal variance assumption may be violated

newy=log(y)
out1=lm(newy~x)

summary(out1)

qqnorm(out1$residuals,main="")
qqline(out1$residuals)

shapiro.test(out1$residuals)

yhat=out1$fitted.values

yhat[order(yhat)]

res=out1$residuals

new_res=res[order(yhat)]

plot(yhat,res,xlab="fitted response", ylab="residual")
abline(h=0,lty=2)
points(yhat[order(yhat)[1:5]],res[order(yhat)[1:5]],col="red")

bartlett.test(new_res,g=c(rep(1,5),rep(2,6)))

# p-value is 0.3099, insignificant

# equal variance assumption no longer violated 