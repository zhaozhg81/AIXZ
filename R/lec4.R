

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
oilchange <- read.csv("http://astro.temple.edu/~zhaozhg/Stat8003/data/speedyoilchange.csv", header=FALSE)
plot( density( oilchange$V1 ) )
oilchange.xbar <- mean( oilchange$V1 )
oilchange.sSq <- var( oilchange$V1 )
oilchange.Tstat <- ( oilchange.xbar - 30 )/( sqrt( oilchange.sSq)/sqrt( length(oilchange$V1) ) ) 
oilchange.pvalue <- pt( oilchange.Tstat, length(oilchange$V1) -1 )

t.test(oilchange,mu=30)


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.3.4
X <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/prostate_X.csv",sep=",")
Y <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/prostate_Y.csv",sep=",")

equalvar <- function(Xi, group)
  {
    n1 <- length( which(group==1) )
    n2 <- length( which(group==2) )
    mean1 <- mean(Xi[ group==1] )
    mean2 <- mean(Xi[ group==2] )
    s1 <- var( Xi[group==1] )
    s2 <- var( Xi[group==2] )

    s.pool <- ( (n1-1)*s1 + (n2-1)*s2 )/( n1+n2-2 )
    t.stat <- (mean1-mean2)/(sqrt(s.pool)*sqrt(1/n1+1/n2) )

    pvalue <- 2*pt( -abs(t.stat), n1+n2-2 )
    
  }

unequalvar <- function(Xi, group)
  {
    n1 <- length( which(group==1) )
    n2 <- length( which(group==2) )
    mean1 <- mean(Xi[ group==1] )
    mean2 <- mean(Xi[ group==2] )
    s1 <- var( Xi[group==1] )
    s2 <- var( Xi[group==2] )

    s.unpool <- (s1/n1 +s2/n2 )
    t.stat <- (mean1-mean2)/sqrt(s.unpool)
    df.sat <- (s1/n1+s2/n2)^2/( (s1/n1)^2/(n1-1) + (s2/n2)^2/(n2-1) )
    pvalue <- 2*pt( -abs( t.stat), df.sat )
    
  }
testvariance <- function(Xi, group )
  {
    n1 <- length( which(group==1) )
    n2 <- length( which(group==2) )
    s1 <- var( Xi[group==1] )
    s2 <- var( Xi[group==2] )
    
    F.stat <- s1/s2
    pvalue <- 2*(F.stat <= 1)*pf( F.stat, n1-1, n2-1 ) +  2*(F.stat>1) * (1-pf( F.stat,n1-1,n2-1) )     
  }

pvalue.equalvar <- apply( X, 1, equalvar, Y)
pvalue.unequalvar <- apply( X, 1, unequalvar, Y)
pvalue.ftest <- apply( X, 1, testvariance, Y)

t.test( X[1, which(Y==1)], X[1, which(Y==2)], var.equal=TRUE, conf.level=0.99)



########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.3.5
teachers <- read.table("http://astro.temple.edu/~zhaozhg/Stat8003/data/English_greek.csv",header=TRUE, sep=",")
diff <- teachers$English - teachers$Greek
T.stat <- ( mean(diff) )/sqrt( var(diff)/length(diff) )

t.test( teachers$English, teachers$Greek, paired=TRUE)
