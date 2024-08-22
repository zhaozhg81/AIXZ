mydata=read.table("C:/Users/dongy/Desktop/linear regression/shortleaf.txt",header=T)
# change the directory to your own

str(mydata)
y=mydata$Vol
x=mydata$Diam

newy=log(y)
newx=log(x)

myfit1=lm(y~x)
myfit2=lm(newy~newx)

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