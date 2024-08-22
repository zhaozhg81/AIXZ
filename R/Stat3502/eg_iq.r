mydata=read.table("C:/Users/dongy/Desktop/linear regression/iq.txt",header=T)
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

out1=lm(y~x1)

anova(out1,out)
