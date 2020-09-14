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

