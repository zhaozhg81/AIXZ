library(mvtnorm)

set.seed(3)

n=20
mu=c(0,0)
sig=matrix(c(1,.9,.9,1),2,2)

x=rmvnorm(n,mu,sig)

x=rbind(x,c(1,-1))

x1=x[,1]
x2=x[,2]

plot(x1[-21],x2[-21],xlab="x1",ylab="x2")
points(1,-1,col="red",pch=4)

newx=cbind(1,x)

h=newx%*%solve(t(newx)%*%newx)%*%t(newx)

lev=diag(h)