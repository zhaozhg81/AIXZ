mydata=read.table("data/3502/depression.txt",header=T)

head(mydata)

y=mydata$y
x1=mydata$age
x2=mydata$x2
x3=mydata$x3

# x2 is the indicator for receiving treatment A 
# x2=1 means receiving A, x2=0 means not receiving A

# x3 is the indicator for receiving treatment B 
# x3=1 means receiving B, x3=0 means not receiving B

# treatment has three levels A, B, C
# need 2 indicators!

n=length(y)
ind2=(1:n)[x2==1]
ind3=(1:n)[x3==1]


plot(x1,y,xlab="age",ylab="effectiveness")
points(x1[ind2],y[ind2],col="red")
points(x1[ind3],y[ind3],col="blue")

legend("bottomright", inset=.05, pch=c(1,1,1),
       c("A","B","C"),  col=c("red", "blue","black"))




###########################################################

x4=x1*x2
x5=x1*x3

out=lm(y~x1+x2+x3+x4+x5)

summary(out)

# all the main effects and the interaction effects are significant!

plot(out$fitted.values,out$residuals,xlab="fitted response",ylab="residual")
abline(h=0,lty=2)


# there is no significant trend in the residual plot
# suggesting interaction model fits the data well

qqnorm(out$residuals,main="")
qqline(out$residuals)

# qq plot looks straight, suggesting the error is normal 

shapiro.test(out$residuals)

# the p-value is greater than .05 from shapiro test, suggesting error is normal




tmp=(summary(out))$coefficients
beta_hat=tmp[,1]


plot(x1,y,xlab="age",ylab="effectiveness")
points(x1[ind2],y[ind2],col="red")
points(x1[ind3],y[ind3],col="blue")
abline(a=beta_hat[1],b=beta_hat[2])
abline(a=beta_hat[1]+beta_hat[3],b=beta_hat[2]+beta_hat[5],col="red")
abline(a=beta_hat[1]+beta_hat[4],b=beta_hat[2]+beta_hat[6],col="blue")

legend("bottomright", inset=.05, pch=c(1,1,1),lty=c(1,1,1),
       c("A","B","C"),  col=c("red", "blue","black"))

mydata = within( mydata, TRT <- relevel(TRT, ref="C") )


mydata$TRT = as.factor(mydata$TRT)
lm.fit = lm(y~age + relevel( TRT, ref="C"), data=mydata)

lm.fit2 = lm(y~age * relevel(TRT, ref="C"), data=mydata)