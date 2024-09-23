mydata=read.table("data/3502/iq.txt",header=T)
# change the directory to your own

str(mydata)
y=mydata$PIQ
x1=mydata$Brain
x2=mydata$Height
x3=mydata$Weight

out=lm(y~x1+x2+x3)

summary(out)

2*(1-pt(1.768,df=34))

sum((out$residuals)^2)

cor(out$fitted.values,y)

newdata = data.frame(x1=95, x2=70, x3=180)
predict(out, newdata=newdata, interval="confidence")

predict(out, newdata=newdata, interval="prediction")

out1=lm(y~x1)

anova(out1,out)
