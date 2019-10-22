

income.all <- read.csv("kaggle_income.csv")
dim(income.all)
head(income.all)

hist(income.all$Median)

income <- income.all$Median[ (income.all$Median >0 ) &( income.all$Median < 300000)  ]


Y <- log( income )
mu.hat <- mean(Y)
sigma.hat <- sqrt( var( Y ) )

mean.est <- exp( mu.hat + sigma.hat^2/2 )


numSim <- 5000
n <- 100

## Log-normal model

risk.lognormal <- function( mu, sigma, numSim=10000)
{
    ## mu <- 1; sigma <- 1
    loss <- array(0, c(2, numSim ) )
    true.mean <- exp( mu + sigma^2/2 )
    
    for( numsim in 1:numSim )
    {
        X.sim <- exp( rnorm( n, mu, sigma) )
        loss[1, numsim ] <- ( mean(X.sim) - true.mean )^2
        Y.sim <- log( X.sim )
        loss[2, numsim ] <- ( exp( mean(Y.sim) + var(Y.sim)/2)  - true.mean )^2
    }
    risk <- apply(loss,1,mean)
    risk
}

mus <- c(0:10)/5
risk.all <- array(0, c(2, length(mus ) ) )
for( i in 1:length(mus) )
{
    risk.all[, i ] <- risk.lognormal( mus[i], 1, numSim=numSim )
}

plot( mus, risk.all[1, ], col='red','l')
points( mus, risk.all[2,], col='green','l')
legend(x=0.5, y= 2.0, c("Without Log", "Log"), col=c("red","green"), lty=c(1,1) )


## Gamma model

risk.gamma <- function( alpha, beta, numSim=10000)
{
    ## mu <- 1; sigma <- 1
    loss <- array(0, c(2, numSim ) )
    true.mean <- alpha * beta
    
    for( numsim in 1:numSim )
    {
        X.sim <- rgamma( n, alpha, beta )
        loss[1, numsim ] <- ( mean(X.sim) - true.mean )^2
        Y.sim <- log( X.sim )
        loss[2, numsim ] <- ( exp( mean(Y.sim) + var(Y.sim)/2)  - true.mean )^2
    }
    risk <- apply(loss,1,mean)
    risk
}

alphas <- c(1:10)/2
risk.all <- array(0, c(2, length(alphas ) ) )
for( i in 1:length(alphas) )
{
    risk.all[, i ] <- risk.gamma( alphas[i], 2, numSim=numSim )
}

plot( alphas, risk.all[1, ], col='red','l')
points( alphas, risk.all[2,], col='green','l')
legend(x=2, y= 30, c("Without Log", "Log"), col=c("red","green"), lty=c(1,1) )




#### Carbo example
data(cereals)
carbo <- cereals$carbo[ setdiff(1:dim(cereals)[1], 58) ]

mean(carbo)
LCL <- mean(carbo) - qnorm( alpha/2, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )
UCL <- mean(carbo) + qnorm( alpha/2, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )


### KDE and empirical distribution function
data(faithful)
X <- faithful$eruptions
res <- hist(X, breaks=20, freq=FALSE )



###############
## KDE for the faithful dataset


X <- faithful$eruptions
n <- length(X)
h <- 1.06 * sqrt( var(X) ) / (n^(1/5))
##########################
## Assume the uniform kernel
xaxis <- seq( min(X), max(X), 0.01 )
funif.hat <- xaxis
for( i in 1:length( xaxis) )
  {
    funif.hat[i] <- sum( abs( xaxis[i] - X)/h <= 1 )/(2*n*h)
  }

### Assume the normal kernel
h <- 0.2
fnorm.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h ))
  }

par(mfrow=c(2,2))

## postscript( "figure/gyser_kde_unif_norm.eps", horizontal=FALSE)
hist( X, freq=F, br=40,  main="Eruptions", xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, ylim=c(0, 1.2) )
points( xaxis, funif.hat, 'l', col='red')
points(xaxis, fnorm.hat, 'l', col='green')
##  dev.off()

########################################################################################################################################################
########################################################################################################################################################
## Effect of the bandwidth
h.small=0.05
fnorm.small.h.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.small.h.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h.small ))
  }

h.large=1
fnorm.large.h.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.large.h.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h.large ))
  }
## postscript("figure/geyser_kde_small_h.eps", horizontal=FALSE)
plot( xaxis, fnorm.small.h.hat, main=paste("h=",h.small, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l' )
## dev.off()

## postscript("figure/geyser_kde_optimal_h.eps", horizontal=FALSE)
plot( xaxis, fnorm.hat, main=paste("h=",h, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l' )
## dev.off()


## postscript("figure/geyser_kde_large_h.eps", horizontal=FALSE)
plot( xaxis, fnorm.large.h.hat, main=paste("h=",h.large, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l' )
## dev.off()


## Choose the bandwidth according tothe maximum likelihood cross validation
library(kedd)
h.cv <- h.mlcv(X)$h
fnorm.cv.hat <- xaxis
for( i in 1:length( xaxis ) )
  {
    fnorm.cv.hat[i] <- mean( dnorm( xaxis[i]-X, 0, h.cv ))
  }

##postscript("figure/geyser_kde_cv_h.eps", horizontal=FALSE)
hist( X, freq=F, br=40,  main="Eruptions", xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, ylim=c(0, 1.2) )
points( xaxis, fnorm.hat, main=paste("h=",h, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l', col='red', lwd=3 )
points( xaxis, fnorm.cv.hat, main=paste("h=",h.cv, sep=""), xlab="Eruptions", ylab="density", cex.main=2, cex.lab=1.5, 'l', col='green',lwd=3 )
legend(2.5, 1.0, c("Silverman", "Cross-validation"), lty=c(1, 1), col=c('red', 'green'), lwd=c(3,3), cex=2 )
##dev.off()

res <- density( X, bw=h.cv, kernel="gaussian", from=min(X), to=max(X) )
plot(res, ylim=c(0,1.2))
points(xaxis, fnorm.cv.hat, 'l', col='green' )

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

#### Simulated example, gamma distribution
X.gamma <- rgamma(100, 2, 2)
xaxis.gamma <- seq( min(X.gamma), max(X.gamma), 0.01)
emp.cdf.gamma <- xaxis.gamma
cdf.gamma <- xaxis.gamma
for(i in 1:length( xaxis.gamma ) )
  {
    emp.cdf.gamma[i] <- mean( X.gamma <= xaxis.gamma[i] )
    cdf.gamma[i] <- pgamma( xaxis.gamma[i], 2, 2)
  }

plot( xaxis.gamma, emp.cdf.gamma, main="Waiting", xlab="Waiting", ylab="cdf", cex.main=2, cex.lab=1.5,'l', lwd=3, col='red' )
points( xaxis.gamma, cdf.gamma, 'l', col='green', lwd=3  )
legend(1.0, 0.4, c("Empirical cdf", "cdf"), lty=c(1, 1), col=c('red', 'green'), lwd=c(3,3), cex=2 )


## Empirical cdf for faithful data
X <- faithful$eruptions
xaxis <- seq( min(X), max(X), 0.01)
emp.cdf <- xaxis

for(i in 1:length( xaxis ) )
  {
    emp.cdf[i] <- mean( X <= xaxis[i] )
   }

plot( xaxis, emp.cdf, main="Faithful", xlab="X", ylab="cdf", cex.main=2, cex.lab=1.5,'l', lwd=3, col='red' )

