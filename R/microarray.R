
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.3.4
X <- read.table("./data/prostate_X.csv",sep=",")
Y <- read.table("./data/prostate_Y.csv",sep=",")

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


par(mfrow=c(1,2))
## Consider gene 3
X3.control <- X[3,][Y==1] 
X3.treat <- X[3,][Y==2]
den.control.1 <- density(X3.control )
den.treat.1 <- density(X3.treat )
plot( den.control.1$x, den.control.1$y, 'l', main="Density under two conditions", xlab="expression", ylab="density", cex.main=2, cex.lab=1.5, col='red',lwd=3 )
points( den.treat.1$x, den.treat.1$y, 'l',  col='green', lwd=3 )
legend(x=1, y=0.3, col=c("red","green"), c("Control","Treatment"), lty=c(1,1), lwd=c(3,3), cex=1 )

ks.test( X3.control, 'pnorm', mean(X3.control), sqrt(var(X3.control)) )
ks.test( X3.treat, 'pnorm', mean(X3.treat), sqrt(var(X3.treat))  )


## Consider gene 6032
X6032.control <- X[6032,][Y==1]
X6032.treat <- X[6032,][Y==2]
den.control.1 <- density(X6032.control )
den.treat.1 <- density(X6032.treat )
plot( den.control.1$x, den.control.1$y, 'l', main="Density under two conditions", xlab="expression", ylab="density", cex.main=2, cex.lab=1.5, col='red',lwd=3 )
points( den.treat.1$x, den.treat.1$y, 'l',  col='green', lwd=3 )
legend(x=0, y=0.8, col=c("red","green"), c("Control","Treatment"), lty=c(1,1), lwd=c(3,3), cex=1 )
ks.test( X6032.control, 'pnorm', mean(X3.control), sqrt(var(X3.control)) )
ks.test( X6032.treat, 'pnorm', mean(X3.treat), sqrt(var(X3.treat))  )

ks.test( X6032.control, X6032.treat)
ks.test(X3.control, X3.treat)

