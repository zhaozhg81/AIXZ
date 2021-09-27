library( kolmim )
source("./R/func/EM.R")


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



## Gaussian Mixture Model
em.esti <- EM( X, K=2, pi.ini= c(0.5,0.5), mu.ini=c(-1,1), sigma.ini=c(1,1), verbose=TRUE )

par(mfrow=c(1,2))
plot( xaxis, em.esti$pi.esti[1] * pnorm( xaxis, em.esti$mu.esti[1], em.esti$sigma.esti[1]) + em.esti$pi.esti[2] * pnorm(xaxis, em.esti$mu.esti[2], em.esti$sigma.esti[2]), 'l', col='red' )
points( xaxis, emp.cdf, col='green', 'l') 

hist(X, breaks=20, freq=FALSE )
points( xaxis, em.esti$pi.esti[1] * dnorm( xaxis, em.esti$mu.esti[1], em.esti$sigma.esti[1]) + em.esti$pi.esti[2] * dnorm(xaxis, em.esti$mu.esti[2], em.esti$sigma.esti[2]), 'l', col='red' )
points(xaxis, fnorm.cv.hat, 'l', col='green' )



## KS test for mixture of normal
ks.res = ks_test_GMM( X, 2, em.esti$pi.esti, em.esti$mu.esti, em.esti$sigma.esti )
  
library(kolmim)
1-pkolm( ks.res$ks.stat, n)
