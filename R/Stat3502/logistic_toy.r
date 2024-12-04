

mydata=read.table("data/3502/toy.txt")

x=mydata$x
y=mydata$y

#############################################################
# linear regression does not work

ab=(lm(y~x))$coefficients

par(mex=.79)

plot(x,y,xlim=c(-.1,1.1),ylim=c(-.5,1.5))

plot(x,y,xlim=c(-.1,1.1),ylim=c(-.5,1.5))
abline(ab[1],ab[2],col="red")


#############################################################
# method 1 to draw the estimated probability (logistic curve)

out=glm(cbind(y,1-y)~x,family=binomial)

ab=out$coefficients

x0=seq(-.1,1.1,by=.01)

y0=exp(ab[1]+ab[2]*x0)/(1+exp(ab[1]+ab[2]*x0))

plot(x0,y0,type="l",xlab="x",ylab="y",xlim=c(-.1,1.1),ylim=c(-.5,1.5),col="red")

points(x,y)

#############################################################
# method 2 to draw the estimated probability (logistic curve)

out=glm(y~x,family=binomial)

summary(out)

p_hat=out$fitted.values
#exp(ab[1]+ab[2]*x)/(1+exp(ab[1]+ab[2]*x))

index=order(x)
plot(x[index],p_hat[index],type="l",xlab="",ylab="",ylim=c(0,1))
points(x,y)


