
income.all <- read.csv("data/kaggle_income.csv")
dim(income.all)
head(income.all)

hist(income.all$Median)

income <- income.all$Median[ (income.all$Median >0 ) &( income.all$Median < 300000)  ]
n <- length(income)




## Plot the density function and the likelihood function of N(\theta, 1)
##              theta=1
##              X=1
par(mfrow=c(1,2))
x <- c(1:1000)/100
## postscript( file="./figure/density.eps",horizontal=FALSE)
plot(x, dexp( x, 1), 'l', col='red', xlab="x", ylab="density", main="pdf", cex.lab=1.5, cex.main=2)
## dev.off()

lambda.all <- c(1:1000)/100
## postscript( file="./figure/likelihood.eps",horizontal=FALSE)
plot(lambda.all, dexp( 1, lambda.all), 'l', col='red', xlab="lambda", ylab="likelihood", main="Likelihood", cex.lab=1.5, cex.main=2)
## dev.off()


Y <- log( income )
mu.hat <- mean(Y)
sigma.hat <- sqrt( var( Y ) )

mean.est <- exp( mu.hat + sigma.hat^2/2 )

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.6.1
## income 


par( mfrow=c(2,2) )
## Sort the data
income.sort <- sort(income, decreasing=FALSE )
## Normal
mu <- mean(income.sort)
sigma <- sqrt( var(income.sort) )
q.norm <- qnorm( (c(1:n)-0.5)/n, mu, sigma, lower.tail=TRUE )
plot( q.norm, income.sort, main="Normal" )
points(c( q.norm[1], q.norm[n]), c(q.norm[1], q.norm[n]), 'l',col='red', lwd=2)

## Log Normal
## Sort the data
l.income.sort <- sort(log(income), decreasing=FALSE )
## Normal
l.mu <- mean(l.income.sort)
l.sigma <- sqrt( var(l.income.sort) )
l.q.norm <- qnorm( (c(1:n)-0.5)/n, l.mu, l.sigma, lower.tail=TRUE )
plot( l.q.norm, l.income.sort, main="Log Normal")
points(c( l.q.norm[1], l.q.norm[n]), c(l.q.norm[1], l.q.norm[n]), 'l',col='red', lwd=2)

#######################
## Fit the data by using gamma distribution
## MLE Estimator
library(rGammaGamma)
esti <- gammaMLE(income)
alpha.hat <- esti[1]
beta.hat <- esti[2]

q.gamma <- qgamma( (c(1:n)-0.5)/n, shape = alpha.hat, scale = beta.hat, lower.tail= TRUE )
plot(  q.gamma, income.sort, main="Gamma")
points( c(q.gamma[1], q.gamma[n]), c(q.gamma[1], q.gamma[n]), 'l', col='red', lwd=2)



## Fit the data using Gaussian Mixture distribution
em.esti <- EM( log(income), K=2, pi.ini= c(0.5,0.5), mu.ini=c(8,10), sigma.ini=c(1,1), verbose=TRUE )

xaxis <- seq( min(log(income)), max(log(income)), 0.01)
emp.cdf <- xaxis
for(i in 1:length( xaxis ) )
  {
    emp.cdf[i] <- mean( log(income) <= xaxis[i] )
  }


par(mfrow=c(2,2))
cdf.fitted.gaussian.mixture =  em.esti$pi.esti[1] * pnorm( xaxis, em.esti$mu.esti[1], em.esti$sigma.esti[1]) + em.esti$pi.esti[2] * pnorm(xaxis, em.esti$mu.esti[2], em.esti$sigma.esti[2])
plot( xaxis, cdf.fitted.gaussian.mixture, 'l', col='red', xlab="", ylab="", main="CDF, gaussian Mixture, K=2" )
points( xaxis, emp.cdf, col='green', 'l') 

hist(log(income), breaks=20, freq=FALSE, xlab="", ylab="", main="PDF: Gaussian Mixture, K=2")
points( xaxis, em.esti$pi.esti[1] * dnorm( xaxis, em.esti$mu.esti[1], em.esti$sigma.esti[1]) + em.esti$pi.esti[2] * dnorm(xaxis, em.esti$mu.esti[2], em.esti$sigma.esti[2]), 'l', col='red' )

cdf.fitted.gaussian = pnorm( xaxis, l.mu, l.sigma)
plot(xaxis, cdf.fitted.gaussian, 'l', col='red', xlab="", ylab="", main="CDF, Log-Normal")
points( xaxis, emp.cdf, col='green', 'l') 

hist(log(income), breaks=20, freq=FALSE, xlab="", ylab="", main="PDF: Log-Normal")
points( xaxis, dnorm(xaxis, l.mu,l.sigma), 'l', col='red' )


max( abs( emp.cdf - cdf.fitted.gaussian.mixture ) )
max( abs( emp.cdf - cdf.fitted.gaussian ) )



qqnorm(income)
qqline(income,col='red', lwd=2)

## Shapiro-Wilk test

shapiro.test(sample(income, 1000) )
shapiro.test(log(sample(income, 1000 )))

## K-S Test
ks.test( income, 'pnorm', mean(income), sd(income ) )
ks.test( log(income), 'pnorm', mean( log(income) ), sd( log(income) ) )

## K-S Test for gamma distribution

ks.test(income, 'pgamma', alpha.hat,1/beta.hat )

hist(log(income), freq=FALSE)
## Gaussian mixture model
income.em.esti <- EM( log(income), K=2, pi.ini= c(0.5,0.5), mu.ini=c(9,11), sigma.ini=c(1,1), verbose=TRUE )

ks.res = ks_test_GMM( log(income), 2, income.em.esti$pi.esti, income.em.esti$mu.esti, income.em.esti$sigma.esti )
