library(MASS)
library(lmtest)


ceb <- read.table("data/ceb.txt", header=TRUE)
ceb$y <- floor( ceb$y )

## Pois
ceb.pois <- glm( y~educ + res, offset=log(n), family=poisson("log"), data=ceb )
summary( ceb.pois )


## Cook's distance
p <- 5
cook.dist <- cooks.distance( ceb.pois )
## postscript("cook_ceb_pois.eps", horizontal=FALSE)
dotchart( pchisq( cook.dist, p+1) )
## dev.off()
outlier <- which( pchisq(cook.dist, p+1)>0.5)

ceb.pois2 <- glm( y~educ + res, offset=log(n), family=poisson("log"), data=ceb[setdiff(c(1:71), outlier),] )
summary( ceb.pois2 )

## Cook's distance after remove the outlier
p <- 5
cook.dist2 <- cooks.distance( ceb.pois2 )
## postscript("cook_ceb_pois2.eps", horizontal=FALSE)
dotchart( pchisq( cook.dist2, p+1) )
## dev.off()

plot( predict( ceb.pois2) , residuals( ceb.pois2) )


## Quasi poisson model
ceb.quasipoisson=glm(formula = y ~ educ + res, family = quasipoisson(link = "log"),      data = ceb, offset = log(n))
summary( ceb.quasipoisson)


## Negative binomial
ceb.nb <- glm.nb( y~  educ + res + offset( log(n) ), data=ceb, link=log)
summary( ceb.nb )

points( predict( ceb.nb), residuals( ceb.nb), col='green')

## ANOVA analysis, do not look at the deviance only
m0 <- update( ceb.nb, .~.-educ-res )
m1 <- update( ceb.nb, .~.-res )
m3 <- update( ceb.nb, .~.-educ )
anova(m0, m1, ceb.nb )
anova(m3, ceb.nb)


## Is it necessary to use negative model?
lrtest( ceb.pois, ceb.nb )

## Confidence interval
cbind( Estimate=coef(ceb.nb), confint(ceb.nb))
exp( cbind( Estimate=coef(ceb.nb), confint(ceb.nb)) )

## Prediction
predict( ceb.nb )


ceb.pois.3 <- glm( y~ dur+educ + res, offset=log(n), family=poisson("log"), data=ceb )
summary(ceb.pois.3)