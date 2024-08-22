## Logistic regression
shuttle <- read.table("./data/shuttle.txt", header=TRUE)

## Using linear model
lm.fit <- lm( ndo~temp, data=shuttle)
lm.coef <- coef( lm.fit )
min.temp <- min(shuttle$temp)
max.temp <- max(shuttle$temp)

plot( shuttle$temp, shuttle$ndo, 'p', ylim=c(-1,3) )
points( c(min.temp, max.temp), c( lm.coef[1] + lm.coef[2]*min.temp, lm.coef[1]+lm.coef[2]*max.temp), 'l', col='red')


## Logistic regression
shuttle.logit <- glm( cbind(ndo, 2-ndo) ~ temp, family=binomial("logit" ), data=shuttle )
summary( shuttle.logit )

predict.logit <- cbind( shuttle$temp, shuttle.logit$fitted.values )
colnames(predict.logit) <- c("temp", "p_i")
predict.logit

## Plot
## postscript(file="shuttle_plot.eps", horizontal=FALSE)
plot( predict.logit[,1], predict.logit[,2], col='red', xlab="temparature", ylab="p_i", main="Logistic regression",lwd=2, cex=1, cex.main=1, cex.lab=1)
## dev.off()

## Odds ratio
exp( coef( shuttle.logit ) )
confint( shuttle.logit, level=0.95)


## deviance analysis
shuttle.logit$df.null
shuttle.logit$df.residual
shuttle.logit$null.deviance
shuttle.logit$deviance

pvalue <- 1-pchisq( shuttle.logit$null.deviance - shuttle.logit$deviance, shuttle.logit$df.null - shuttle.logit$df.residual)
pvalue

## Goodness-of-fit test
gof.pvalue <- 1 - pchisq(shuttle.logit$deviance, shuttle.logit$df.residual)

## Prediction
newdata <- data.frame( temp=28 )
predict( shuttle.logit, newdata = newdata, type="response" )
predict( shuttle.logit, newdata = newdata, type="link" )

newdata <- data.frame( temp=c(20:81) )
pred <- predict( shuttle.logit, newdata= newdata, type= "response" )
pred <- cbind( newdata, pred )
colnames( pred ) <- c("temp","pred.logit")
plot( pred$temp, pred$pred.logit, 'l' )

#####################################################################
###########################################################################
########################################################################


## Probit regression
shuttle.probit <- glm( cbind(ndo, 2-ndo) ~ temp, family=binomial("probit" ), data=shuttle )
summary( shuttle.probit )

predict.probit <- cbind( shuttle$temp, shuttle.probit$fitted.values )
colnames(predict.probit) <- c("temp", "p_i")
predict.probit

## Plot
## postscript(file="shuttle_plot.eps", horizontal=FALSE)
plot( predict.probit[,1], predict.probit[,2], col='red', xlab="temparature", ylab="p_i", main="Probit regression",lwd=4, cex=2, cex.main=2, cex.lab=2)
## dev.off()

## deviance analysis
pvalue <- 1-pchisq(  shuttle.probit$null.deviance - shuttle.probit$deviance, shuttle.probit$df.null - shuttle.probit$df.residual )
pvalue

## goodness-of-fit test
gof.pvalue <- 1 - pchisq( shuttle.probit$deviance, shuttle.probit$df.residual )
gof.pvalue

## Prediction
pred.probit <- predict( shuttle.probit, newdata = newdata, type="response" )
pred <- cbind( pred, pred.probit)
colnames( pred ) <- c("temp", "pred.logit", "pred.probit" )
plot( pred$temp, pred$pred.logit, 'l', col='red', pch=1, xlab="Temparature", ylab="Predicted Probability" )
points( pred$temp, pred$pred.probit, 'l', col='green', pch=2 )

legend( x=30, y=0.4, c("Logit", "Probit"), lty=c(1,1), col=c('red','green') )
