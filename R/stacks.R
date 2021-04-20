library(R2OpenBUGS)
library(coda)

Y <- c(42,37,37,28,18,18,19,20,15,14,14,13,11,12,8,7,8,8,9,15,15)
x <- t( array( c(80,27,89,
           80,27,88,
           75,25,90,
           62,24,87,
           62,22,87,
           62,23,87,
           62,24,93,
           62,24,93,
           58,23,87,
           58,18,80,
           58,18,89,
           58,17,88,
           58,18,82,
           58,19,93,
           50,18,89,
           50,18,86,
           50,19,72,
           50,19,79,
           50,20,80,
           56,20,82,
           70,20,91), c(3, 21)
              )
)

N <- 21
p <- 3
data <- list("N", "p", "Y", "x")
inits <- function(){
  list( beta0= rnorm(1, 0, 1), beta= rnorm(3, 0.2, 0.1), tau=dgamma(1, 2, 2) )
                   }
parameters <- c("beta0", "beta", "tau")

stacks.gibbs <- bugs( data, inits, parameters, "stacks.txt", n.iter=30000, n.burnin=10000, n.thin=20  )

mcmc.stacks=as.mcmc.list( stacks.gibbs )

## save(file="stacks.Rdata",mcmc.stacks)
## load("stacks.Rdata")

densityplot( mcmc.stacks )
acfplot( mcmc.stacks )
gelman.diag( mcmc.stacks )

stacks.summary <- summary( mcmc.stacks, quantile=c(0.025,0.5,0.975) )
stacks.summary$quantile[,2]
