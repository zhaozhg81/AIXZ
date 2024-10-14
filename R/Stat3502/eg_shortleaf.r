mydata=read.table("data/3502/shortleaf.txt",header=T)
# change the directory to your own

str(mydata)
y=mydata$Vol
x=mydata$Diam

plot(x, y)

newy=log(y)
newx=log(x)

plot(newx, newy )


myfit1=lm(y~x)

myfit.tmp1= lm(y~log(x))
summary(myfit.tmp1)
plot( myfit.tmp1$fitted.values, myfit.tmp1$residuals)

myfit.tmp2= lm(log(y)~x)
summary(myfit.tmp2)
plot( myfit.tmp2$fitted.values, myfit.tmp2$residuals)



myfit2=lm(newy~newx)
plot(myfit2$fitted.values, myfit2$residuals)

ab1=myfit1$coefficients
ab2=myfit2$coefficients

par(mfrow=c(1, 2))

plot(x,y,xlab="Diam",ylab="",cex.lab=1.2,main="Before")
title(ylab="Vol", mgp=c(2,1,0), cex.lab=1.2)
abline(a=ab1[1],b=ab1[2],col="red")

plot(newx,newy,xlab="log Diam",ylab="",cex.lab=1.2,main="
     After")
title(ylab="log Vol", mgp=c(2,1,0), cex.lab=1.2)
abline(a=ab2[1],b=ab2[2],col="red")

qqnorm( myfit1$residuals )
qqline( myfit1$residuals,col='red', lwd=2)

qqnorm( myfit2$residuals )
qqline( myfit2$residuals,col='red', lwd=2)


shapiro.test( myfit1$residuals )

shapiro.test( myfit2$residuals )

datanew = data.frame( newx= log(10))

predict( myfit2, newdata= datanew, interval='confidence')
exp( predict( myfit2, newdata= datanew, interval='confidence') )

predict( myfit2, newdata= datanew, interval='predict')
exp(predict( myfit2, newdata= datanew, interval='predict') )

