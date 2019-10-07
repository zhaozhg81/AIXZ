

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.3.5
teachers <- read.table("data/English_greek.csv",header=TRUE, sep=",")
diff <- teachers$English - teachers$Greek
T.stat <- ( mean(diff) )/sqrt( var(diff)/length(diff) )

t.test( teachers$English, teachers$Greek, paired=TRUE)





########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.4.1
observ <- c(102, 82, 16)
p.null <- c(0.45,0.4,0.15)
expected <- 200*p.null

chi.stat <- sum( (observ-expected)^2/expected )
p.value <- 1-pchisq( chi.stat, 2)


marketing <- chisq.test( observ, p=p.null)


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Example 4.4.2
## Exit poll
poll1 = read.table( "data/ExitPoll1.txt", header=TRUE )
x1 <- poll1[1:3, 1:3 ]
x1.exp <- array( c( x1[1,3]*x1[3,1]/x1[3,3], x1[2,3]*x1[3,1]/x1[3,3], x1[3,1], x1[1,3]*x1[3,2]/x1[3,3], x1[3,2]*x1[2,3]/x1[3,3], x1[3,2], x1[1,3],x1[2,3],x1[3,3]), c(3, 3) )
test.stat <- sum( (x1[1:2,1:2]-x1.exp[1:2,1:2])^2/x1.exp[1:2,1:2] )
poll.test <- chisq.test( x1[1:2,1:2], correct=FALSE )

##
poll2 <- read.table( "data/ExitPoll2.txt", header=TRUE )
x2 <- poll2[1:6, 1:3]
poll2.test <- chisq.test( x2[1:5,1:2] )





########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
## Example 4.5.1

# ------------------------------------------------------------------------
# Teenage Suicide Example: Hypothesis Testing
# ------------------------------------------------------------------------

suicide <-  c(1867,1789,1944,2094,2097,1981,1887,2024,1928,2039,1978,1859)
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
month <- 1:12
names(month) <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
dat <- data.frame(month,days,suicide,suicide/days)
names(dat) <- c("month","days","suicide","rate per day")
dat

#postscript(file=".figure/suicide.eps",horizontal=FALSE)
par(mfrow = c(1,2),oma = c(.5,.5,.5,.5))
plot(1:12,suicide,type = "b",ylab = "Number of Teenage Suicide",
     xlab = "Month", xaxt = "n")
axis(1, at = 1:12, labels = names(month))
plot(1:12, suicide/days, type="b", ylab="Rate of Tennage suicides",
     xlab = "Month", xaxt="n")
axis(1, at=1:12, labels = names(month))
#dev.off()

eio <- days/365*sum(suicide)
two.log.lambda <- 2*sum(suicide*(log(suicide)-log(eio)))
cat("two.log.lambda is", two.log.lambda,"\n")

cutoff <- qchisq(.95,11)
cat("Cutoff is", cutoff, "\n")

p.val <- 1 - pchisq(two.log.lambda,11)
cat("pvalue is", p.val, "\n")


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
