

library(R2OpenBUGS)
library(coda)
library(lattice)

baseball <- read.table("./data/EfronMorrisBB.txt", header=TRUE)
y <- baseball[,5]
J <- length(y)

data <- list("J", "y")
inits <- function(){
  list( theta= rnorm(18, 0.2, 0.1), prec.y=rgamma(1,2,2), prec.theta=rgamma(1,2,2), mu.theta=rnorm(1,0,1))
                   }
parameters <- c("theta", "prec.y", "prec.theta")

baseball.gibbs <- bugs( data, inits, parameters,
                        "/home/zhaozhg/Dropbox/programming/github/AIXZ/R/baseball.txt", 
                        n.iter=50000, n.burnin=10000, n.thin=40 )

## load("baseball_output.Rdata")

mcmc.baseball=as.mcmc.list( baseball.gibbs )
##save(file="Baseball.Rdata", mcmc.baseball)
##load(file="Baseball.Rdata")

## postscript("../figure/baseball_density.eps", horizontal=FALSE)
densityplot( mcmc.baseball )
## dev.off()

## postscript("../figure/baseball_acf.eps", horizontal=FALSE)
acfplot( mcmc.baseball )
## dev.off()

gelman.diag( mcmc.baseball )

baseball.summary <- summary( mcmc.baseball, quantile=c(0.025,0.5,0.975) )
yPred= baseball.summary$quantiles[ c(4,14:21,5:13),2]
Season.Ave=baseball[,10]
sum( (yPred - Season.Ave)^2 )
sum( (y -Season.Ave)^2 )
sum( ( mean(y) - Season.Ave)^2 )

## postscript("../figure/prediction.eps",horizontal=FALSE)
plot(c(1:18), y, xlab="Players", ylab="Prediction")
points(c(1:18), yPred,col='red')
points(c(1:18), Season.Ave,col='green')
legend(x=10, y=.4, c("y", "yPred", "Season Ave"), col=c("black","red","green"), pch=c("o","o","o") )
## dev.off()
