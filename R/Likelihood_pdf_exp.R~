

## Plot the density function and the likelihood function of N(\theta, 1)
##              theta=1
##              X=1
par(mfrow=c(1,2))
x <- c(1:1000)/100
postscript( file="./figure/density.eps",horizontal=FALSE)
plot(x, dexp( x, 1), 'l', col='red', xlab="x", ylab="density", main="pdf", cex.lab=1.5, cex.main=2)
dev.off()

lambda.all <- c(1:1000)/100
## postscript( file="./figure/likelihood.eps",horizontal=FALSE)
plot(lambda.all, dexp( 1, lambda.all), 'l', col='red', xlab="lambda", ylab="likelihood", main="Likelihood", cex.lab=1.5, cex.main=2)
## dev.off()

