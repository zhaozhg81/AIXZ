n <- 100000
LWD=3

## Normal distribution
x.norm <- rnorm(n, 0, 1)
x.axis <- c(1:1000)/100-5
plot( x.axis, dnorm(x.axis, 0, 1), 'l', col='red', xlab="x", ylab="pdf", main="Normal", ylim=c(0,1.5), cex.lab=1.5, cex=2 ,lwd=LWD )
points(x.axis, dnorm(x.axis, 1,1), 'l', col='green' ,lwd=LWD)
points(x.axis, dnorm(x.axis, 1, 2),'l', col='blue'  ,lwd=LWD)
points(x.axis, dnorm(x.axis, 1, 1/3), 'l',col='black' ,lwd=LWD)
legend(x=-4,y=1.2, c('mu=0,sigma=1','mu=1,sigma=1','mu=1,sigma=2','mu=1,sigma=1/3'), lty=c(1,1,1,1), col=c('red','green','blue', 'black'), cex=2, lwd=c(LWD,LWD,LWD,LWD) )

dev.off()

## There are four functions for each given distriubtion, pnorm, qnorm, dnorm, rnorm
## rnorm: generate random numbers;
## pnorm: calculate cdf function;
## qnorm: calculate the quantile;
## dnorm: calculate the density

pnorm( 1, lower.tail= TRUE)
qnorm( 0.05, lower.tail=FALSE)
rnorm( 10 )
dnorm( 0 )


## chisq distribution
x.axis <- c(1:1000)/10
plot( x.axis, dchisq(x.axis, 50), 'l', col='red', xlab="x", ylab="pdf", main="Exponential", ylim=c(0,0.5), cex.lab=1.5, cex=2 ,lwd=LWD )
points(x.axis, dchisq(x.axis, 20), 'l', col='green' ,lwd=LWD)
points(x.axis, dchisq(x.axis, 10),'l', col='blue'  ,lwd=LWD)
points(x.axis, dchisq(x.axis, 2), 'l', col='black' ,lwd=LWD)
legend(x=40,y=0.3, c('df=50','df=20','df=10','df=2'), lty=c(1,1,1,1), col=c('red','green','blue','black'),cex=2 , lwd=c(LWD,LWD,LWD,LWD) )

dev.off()
qchisq(0.95, 3)
pchisq(6,3)

## T distribution
x.norm <- rnorm(n, 0, 1)
x.axis <- c(1:1000)/100-5
plot( x.axis, dt(x.axis, 2), 'l', col='red', xlab="x", ylab="pdf", main="Normal", ylim=c(0,.5), cex.lab=1.5, cex=2  ,lwd=LWD)
points(x.axis, dt(x.axis,10), 'l', col='green' ,lwd=LWD)
points(x.axis, dt(x.axis,100),'l', col='blue'  ,lwd=LWD)
points(x.axis, dnorm(x.axis, 0,1), 'l',col='black' ,lwd=LWD)
legend(x=2,y=.4, c('df=2','df=10','df=100','df=infinity'), lty=c(1,1,1,1), col=c('red','green','blue', 'black'), cex=1 , lwd=c(LWD,LWD,LWD,LWD) )

dev.off()


