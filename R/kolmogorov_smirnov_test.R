



########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.6.1
## income 
income.all <- read.csv("data/kaggle_income.csv")
income <- income.all$Median[ (income.all$Median >0 ) &( income.all$Median < 300000)  ]

n <- length(income)


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


qqnorm(income)
qqline(income,col='red', lwd=2)

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.6.2
par(mfrow=c(2,2))
t.3 <- rt(1000, 3)
t.5 <- rt(1000, 5)
t.10 <- rt(1000, 10)
t.100 <- rt(1000, 100)

qqnorm(t.3)
qqline(t.3,col='red', lwd=2)


qqnorm(t.5)
qqline(t.5,col='red', lwd=2)


qqnorm(t.10)
qqline(t.10,col='red', lwd=2)


qqnorm(t.100)
qqline(t.100,col='red', lwd=2)



########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.7.1

library(kolmim)
income.all <- read.csv("data/kaggle_income.csv")
income <- income.all$Median[ (income.all$Median >0 ) &( income.all$Median < 300000)  ]

n <- length(income)

income.sort <- sort( income, decreasing=FALSE) 

##Normal case
mu <- mean(income.sort)
sigma <- sqrt( var(income.sort) )
cdf.normal <- pnorm( income.sort, mu, sigma )
ks.normal <- max( abs( cdf.normal - c(0:(n-1))/n), abs( cdf.normal-c(1:n)/n) )
1-pkolm( ks.normal,  length(income) )

ks.normal.Rfunc <- ks.test( income, 'pnorm', mu, sigma, exact=TRUE )



########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.7.2
install.packages("devtools")
library(devtools)
devtools::install_github("gaborcsardi/praise")
library(praise)
for(i in 1:10)
  print(praise())


X <- read.table("data/prostate_X.csv",sep=",")
Y <- read.table("data/prostate_Y.csv",sep=",")

par(mfrow=c(1,2))
## Consider gene 3
X3.control <- X[3,][Y==1] 
X3.treat <- X[3,][Y==2]
den.control.1 <- density(X3.control )
den.treat.1 <- density(X3.treat )
plot( den.control.1$x, den.control.1$y, 'l', main="Density under two conditions", xlab="expression", ylab="density", cex.main=2, cex.lab=1.5, col='red',lwd=3 )
points( den.treat.1$x, den.treat.1$y, 'l',  col='green', lwd=3 )
legend(x=2, y=0.4, col=c("red","green"), c("Control","Treatment"), lty=c(1,1), lwd=c(3,3), cex=2 )

ks.test( X3.control, 'pnorm', mean(X3.control), sqrt(var(X3.control)) )
ks.test( X3.treat, 'pnorm', mean(X3.treat), sqrt(var(X3.treat))  )


## Consider gene 6032
X6032.control <- X[6032,][Y==1]
X6032.treat <- X[6032,][Y==2]
den.control.1 <- density(X6032.control )
den.treat.1 <- density(X6032.treat )
plot( den.control.1$x, den.control.1$y, 'l', main="Density under two conditions", xlab="expression", ylab="density", cex.main=2, cex.lab=1.5, col='red',lwd=3 )
points( den.treat.1$x, den.treat.1$y, 'l',  col='green', lwd=3 )
legend(x=2, y=0.4, col=c("red","green"), c("Control","Treatment"), lty=c(1,1), lwd=c(3,3), cex=2 )
ks.test( X6032.control, 'pnorm', mean(X3.control), sqrt(var(X3.control)) )
ks.test( X6032.treat, 'pnorm', mean(X3.treat), sqrt(var(X3.treat))  )

## Shapiro-Wilk test

shapiro.test(income)
shapiro.test(log(income))

###
