
###################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
#### Interpretation of the coverage probability.
library(plotrix)

alpha = 0.05
theta <- 1
rep <- 20
ind <- c(1:rep)
Ls <- ind
Us <- ind
center <- ind
for(i in 1:rep)
{
  Y <- rnorm( 10, theta, 1 )
  center[i] <- mean(Y)
  Ls[i] <- mean(Y) - qnorm( 1-alpha/2) * 1/sqrt( 10 )
  Us[i] <- mean(Y) + qnorm( 1-alpha/2) * 1/sqrt( 10 )
}
plotCI(center, li=Ls, ui=Us, ylab="interval" )
points(c(1,rep), c(1,1), col='red','l')

### Confidence coefficient
theta <- 1
rep <- 2000
ind <- c(1:rep)
Ls <- ind
Us <- ind
for(i in 1:rep)
{
  Y <- rnorm( 10, theta, 1 )
  Ls[i] <- mean(Y) - qnorm( 1-alpha/2) * 1/sqrt( 10 )
  Us[i] <- mean(Y) + qnorm( 1-alpha/2) * 1/sqrt( 10 )
}
cov <- mean( (theta>Ls)*(theta<Us))

## Exponential model
alpha = 0.05
theta <- 1
rep <- 20
ind <- c(1:rep)
Ls <- ind
Us <- ind
center <- ind

for(i in 1:rep)
{
  Y <- rexp( 1, theta, 1 )
  Ls[i] <- Y/2.996
  Us[i] <- Y/0.051
}
plotCI(center, li=Ls, ui=Us, ylab="interval" )
points(c(1,rep), c(1,1), col='red','l')

