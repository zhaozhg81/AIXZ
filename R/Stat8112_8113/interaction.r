n=76

set.seed(1)

x1=runif(n,min=0,max=10)
x2=rbinom(n,size=1,prob=.5)

y=10-x1-10*x2+2*x1*x2+rnorm(n)

ind=(1:n)[x2==1]

plot(x1,y)
points(x1[ind],y[ind],col="red")
points(x1[-ind],y[-ind],col="blue")

legend("top", inset=.05, pch=c(1,1),
       c("x2=0","x2=1"),  col=c("blue","red"),cex=.5)


#######################################################

out=lm(y~x1+x2)
tmp=(summary(out))$coefficients


beta_hat=tmp[,1]

plot(x1,y)
points(x1[ind],y[ind],col="red")
points(x1[-ind],y[-ind],col="blue")
abline(a=beta_hat[1],b=beta_hat[2],col="blue")
abline(a=beta_hat[1]+beta_hat[3],b=beta_hat[2],col="red")

legend("top", inset=.05, pch=c(1,1),lty=c(1,1),
       c("x2=0","x2=1"), col=c("blue","red"),cex=.5)


plot(out$fitted.values,out$residuals,xlab="fitted response",ylab="residual")
abline(h=0,lty=2)


# there is significant trend in the residual plot
# suggesting model does not fit the data well

summary(out)

# the estimated regression coefficients (except the intercept) are not significant

################################################
# now we add interaction into the model

x3=x1*x2

out_int=lm(y~x1+x2+x3)

summary(out_int)

# now the estimated regression coefficients are significant!

plot(out_int$fitted.values,out_int$residuals,xlab="fitted response",ylab="residual")
abline(h=0,lty=2)


# there is no significant trend in the residual plot
# suggesting interaction model fits the data well



tmp=(summary(out_int))$coefficients
beta_hat=tmp[,1]


plot(x1,y)
points(x1[ind],y[ind],col="red")
points(x1[-ind],y[-ind],col="blue")
abline(a=beta_hat[1],b=beta_hat[2],col="blue")
abline(a=beta_hat[1]+beta_hat[3],b=beta_hat[2]+beta_hat[4],col="red")

legend("top", inset=.05, pch=c(1,1),lty=c(1,1),
       c("x2=0","x2=1"), col=c("blue","red"),cex=.5)

out_int.2=lm(y~x1*x2)
