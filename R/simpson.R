## Simulation
## Two age group, 5-10, 50-55
n1 <- 10
n2 <- 10

x1 <- sample( c(55:75), n1, replace=TRUE)
x1 <- x1-55
y1 <- 10 + 0.8* x1 + rnorm(n1)

x2 <- sample(c(70:85), n2, replace=TRUE)
x2 <- x2-55

y2 <- -10 + 0.8* x2+rnorm(n2)


plot( x1, y1, col='black', xlim=c( min(x1,x2)-1, max(x1,x2)+1), ylim=c( min(y1,y2)-0.1, max(y1,y2)+0.1 ) )
points(x2,y2, col='black')

cor( c(x1,x2), c(y1,y2) )


plot( x1, y1, col='red', xlim=c( min(x1,x2)-1, max(x1,x2)+1), ylim=c( min(y1,y2)-0.1, max(y1,y2)+0.1 ) )
points(x2,y2, col='green')


## Berkeley admission data
berkeley <- read.csv("data/berkeley.csv")

Accepted <- tapply( berkeley$Admission=="Accepted", list( berkeley$Major, berkeley$Gender), sum )
Rejected <- tapply( berkeley$Admission=="Rejected", list( berkeley$Major, berkeley$Gender), sum )

cond.accept.rate <- Accepted/(Accepted + Rejected )
marg.accept.rate <- apply( Accepted, 2, sum)/( apply(Accepted+Rejected,2,sum) )



## This is the estimator after ajusting for the confounding variable.
total.female <- apply( Accepted + Rejected, 2, sum )[1]
total.male <- apply( Accepted + Rejected, 2, sum )[2]

applicant.ratio <- apply(Accepted+Rejected,1,sum)/(total.female+total.male)

sum( cond.accept.rate[,1] * applicant.ratio - cond.accept.rate[,2] * applicant.ratio )
