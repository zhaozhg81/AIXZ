cheese <- read.table("http://astro.temple.edu/~zhaozhg/Stat8113/data/cheese.txt", header=TRUE)


## psudo-manual calculation
X <- data.matrix( cheese[,3:5] )
Y <- data.matrix( cheese[,2] )
PX <- solve( t(X)%*%X )%*% t(X)

X <- cbind( rep(1,30), X)
PX <- solve( t(X)%*%X )%*% t(X)
beta.hat <- PX%*%Y
Hat.matrix <- X %*% PX
residual <- Y - Hat.matrix %*% Y
standard.error <- sqrt( sum( residual^2 )/(n-4) )
covariance.matrix <- solve(t(X)%*%X)*standard.error^2
std.beta <- sqrt( diag( covariance.matrix) )


## Confidence interval for betas
n <- dim(cheese)[1]
lm.fit <- lm( taste ~ Acetic + H2S + Lactic, data = cheese )
summary(lm.fit)

confint( lm.fit, level=0.9)


## Linear contrast
library(multcomp)
K <- matrix( c(0, 1, -2, 0), nrow=1) ## Constrast the vector of the contrast
test1 <- glht( lm.fit, linfct= K) 
summary( test1 ) ## Summary of the test 
confint( test1, level=0.90) ## Construct the confidence interval

newdata <- data.frame( Acetic=6.5, H2S=7.5, Lactic=1.2 )
Pred.Int <- predict( cheese.lm.fit, newdata, interval="predict" , level=0.9)
Conf.Int <- predict( cheese.lm.fit, newdata, interval="confidence" , level=0.9)

newdata2 <- data.frame( Acetic=c(6.5, 10.0), H2S=c(7.5, 2.0), Lactic=c(1.2, 5.5) )
Pred.Int <- predict( cheese.lm.fit, newdata2, interval="predict" , level=0.9)
Conf.Int <- predict( cheese.lm.fit, newdata2, interval="confidence" , level=0.9)


########################################################################
########################################################################
########################################################################
########################################################################

anova( null.model.cheese, full.model.cheese )




########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
########################################################################
## Normality assumption check
resid <- cheese.lm.fit$residuals
plot( resid )
resid.std <- ( resid - mean(resid) )/sqrt( var(resid) )
plot( resid.std )
ks.test( resid.std, 'pnorm' )
