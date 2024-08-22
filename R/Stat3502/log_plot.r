x=seq(-6,6,by=.1)

y=exp(x)/(1+exp(x))

par(mex=.79)

plot(x,y,xlab="z",ylab=expression(pi(z)),lty=1,type="l")


############################

x=seq(-.1,1.1,by=.01)
y=exp(-2+4*x)/(1+exp(-2+4*x))

plot(x,y,xlab="x",ylab=expression(pi(x)),type="l",
     main=expression(paste(a==-2,",",b==4)))

