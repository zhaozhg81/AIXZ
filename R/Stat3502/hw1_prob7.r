y=c(0,1,2,3)

x=c(-.5,.5,2.5,3.5)

out=lm(y~x)

c=out$coefficients

plot(x,y)
abline(a=c[1],b=c[2])

out1=lm(y~x-1)

abline(a=0,b=out1$coefficients,col="red")

sum(x*y)/sum(x^2)

sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)

newx=cbind(1,x)

newy=as.matrix(y)

t(newx)%*%newx

solve(t(newx)%*%newx)

solve(t(newx)%*%newx)%*%t(newx)%*%newy

c

newx%*%solve(t(newx)%*%newx)%*%t(newx)%*%newy

out$fitted.values

out$residuals
