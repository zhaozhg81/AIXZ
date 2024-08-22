library(MASS)
library(pscl)
library(lmtest)


biochem <- read.csv("./data/biochemist.csv")

head(biochem)

## Poisson model

biochem.poi <- glm(art~fem+mar+kid5+phd+ment, family=poisson, data=biochem)
summary( biochem.poi )

pr <- residuals(biochem.poi,"pearson")

## Quasi poisson model
biochem.quasipoi <- glm(art~fem+mar+kid5+phd+ment, family=quasipoisson, data=biochem)


se <- function(model) sqrt(diag(vcov(model)))
round(data.frame(p=coef(biochem.poi), q=coef(biochem.quasipoi), se.p=se(biochem.poi), se.q=se(biochem.quasipoi),
                 ratio=se(biochem.quasipoi)/se(biochem.poi)), 4)


## Negative binomial model
biochem.nb <- glm.nb(art~fem+mar+kid5+phd+ment, data=biochem)
summary(biochem.nb)


round(data.frame(
   p=coef(biochem.poi),q=coef(biochem.quasipoi),nb=coef(biochem.nb),
   se.p=se(biochem.poi),se.q=se(biochem.quasipoi),se.nb=se(biochem.nb)),4)


## Variance function
xb <- predict(biochem.nb) 

g <- cut(xb, breaks=quantile(xb,seq(0,100,5)/100))

m <- tapply(biochem$art, g, mean)
v <- tapply(biochem$art, g, var)


plot(m, v, xlab="Mean", ylab="Variance", 
          main="Mean-Variance Relationship")
mtext("Articles Published by Ph.D. Biochemists",padj=-0.5)
x <- seq(0.63,3.37,0.02)
phi <- sum(pr^2)/df.residual(biochem.poi)
lines(x, x*phi, lty="dashed")
lines(x, x*(1+x/biochem.nb$theta))
legend("topleft", lty=c("dashed","solid"), 
   legend=c("Q. Poisson","Neg. Binom."), inset=0.05)


## Test comparing poison model and negative binomial model.
-2*( logLik( biochem.poi) - logLik(biochem.nb))

lrtest(biochem.poi,biochem.nb)


## zero inflated poisson
zobs <- biochem$art == 0
zpoi <- dpois(0,exp(predict(biochem.poi)))
c(obs=mean(zobs), poi=mean(zpoi))


library(pscl)

biochem.zip <- zeroinfl(art~fem+mar+kid5+phd+ment, data=biochem)
summary(biochem.zip)

pr <- predict(biochem.zip,type="zero")  # π
mu <- predict(biochem.zip,type="count") # μ

zip <- pr + (1-pr)*exp(-mu) # also predict(mzip,type="prob")[,1]
mean(zip)
