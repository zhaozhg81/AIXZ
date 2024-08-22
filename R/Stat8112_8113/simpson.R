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


## Kidney stone data
stone = NULL
stone$Z=c(0,0,0,0,1,1,1,1) ## 0 means small stone, 1 means large stone
stone$X=c(0,0,1,1,0,0,1,1) ## 0 means treatment A, 1 means treatment B
stone$Y=c(81,6,234,36,192,71,55,25) ## Number of recovery vs non-recovery

rates <- NULL
rates$A <- c( stone$Y[1]/(stone$Y[1]+stone$Y[2]), stone$Y[5]/(stone$Y[5]+stone$Y[6]))
rates$B <- c( stone$Y[3]/(stone$Y[3]+stone$Y[4]), stone$Y[7]/(stone$Y[7]+stone$Y[8]))
rates$overall <- c( sum( stone$Y[c(1,5)] )/sum(stone$Y[c(1,2,5,6)]) , sum( stone$Y[c(3,7)] )/sum(stone$Y[c(3,4,7,8)]) )

## Adjusted estimator
P.Z1 <- sum( stone$Z*stone$Y)/sum(stone$Y)
P.Z0 <- sum( (1-stone$Z)*stone$Y)/sum(stone$Y)

E.Y0 <- stone$Y[1]/(stone$Y[1]+stone$Y[2]) * P.Z0 + stone$Y[5]/(stone$Y[5]+stone$Y[6]) * P.Z1
E.Y1 <- stone$Y[3]/(stone$Y[3]+stone$Y[4]) * P.Z0 + stone$Y[7]/(stone$Y[7]+stone$Y[8]) * P.Z1


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

## ATT
P.dept.cond.Female <- (Accepted[,1]+Rejected[,1])/apply( Accepted+Rejected,2,sum)[1]
P.cond.male.dept <- Accepted[,2]/(Accepted[,2]+Rejected[,2])

E.Y0 <- mean( P.cond.male.dept )
E.Y1 <- sum( Accepted[,2])/(sum(Accepted[,2])+sum(Rejected[,2]))
