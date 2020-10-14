setwd("./R/homework/galaxy")
library(MASS)
data( galaxies )
X = galaxies/1000

hist(X, breaks=20)

den=density( X) 
## Plot the density estimation
postscript("density.eps",horizontal=FALSE)
plot(den)
dev.off()

## The bandwidth
den$bw

emp.cdf = array(0, length(X) )
for(i in 1:length(X))
{
  emp.cdf[i] = mean( X <= X[i] )
}

## Plot the empirical cdf
postscript("empcdf.eps",horizontal=FALSE)
plot(X, emp.cdf, 'l' )
dev.off()


## Plot the histogram 
postscript("hist.eps",horizontal=FALSE)
hist(X,breaks=20)
dev.off()



## Gaussian Mixture Model
em.esti <- EM( X, K=3, pi.ini= c(0.3,0.3,0.3), mu.ini=c(10,20,30), sigma.ini=c(1,1,1), verbose=TRUE )

em.cdf = array(0, length(X) )
for(i in 1: length(X)) 
{
  em.cdf[i] = sum( em.esti$pi.esti * pnorm( X[i], em.esti$mu.esti, em.esti$sigma.esti) ) 
}

max.diff = max(  max( abs(em.cdf-emp.cdf)), max( abs(em.cdf - c(0, emp.cdf[1:(length(X)-1)]))) )