
#install.packages("car")

library(car)

mydata=read.table("C:/Users/dongy/Desktop/linear regression/bloodpress.txt",header=T)

head(mydata)

y=mydata$BP
x1=mydata$Age
x2=mydata$Weight
x3=mydata$BSA
x4=mydata$Dur
x5=mydata$Pulse
x6=mydata$Stress

newdata=data.frame(x1,x2,x3,x4,x5,x6,y)

out0=lm(y~.,data=newdata)

vif(out0)

##################

out=lm(x2~x1+x3+x4+x5+x6)
summary(out)$r.squared

1/(1-summary(out)$r.squared)

##########################
#standardize the predictors

av=apply(newdata,2,mean)
sd=apply(newdata,2,sd)

standard_data=data.frame(t((t(newdata)-av)/sd))

out1=lm(y~.,data=standard_data)

vif(out1)


#########################################
# test for significance correlation

cor(newdata)

cor.test(x2,x3)

plot(x2,x3)

##################################
# partial regression plot

mod1=lm(y~x3)
mod1$coefficients

mod2=lm(y~x2+x3)
mod2$coefficients

summary(mod1)$coefficients
summary(mod2)$coefficients


par(mfrow=c(1, 2))

plot(x3,y)

r1=lm(y~x2)$residuals
r2=lm(x3~x2)$residuals

plot(r2,r1,ylab="residual of y on x2", xlab="residual of x3 on x2")


par(mfrow=c(1, 1))