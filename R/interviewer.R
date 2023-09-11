apex <- read.table("./data/apexdata.txt",header=TRUE)

tapply(apex$Evaluation,apex$Officer,mean)

par(mar = c(4.5,4.5,1,1))
boxplot(Evaluation ~ Officer, data = apex, xlab="Interviewer", ytab="Rating")

library("nlme")
fm1 <- lme(Evaluation ~ 1, data = apex, random = ~ 1|Officer)
summary(fm1)


## Model diagnosis
## Residual plot
plot( fitted( fm1 ), residuals( fm1 ) )
## KS test
ks.test( residuals(fm1)/ sqrt( var( residuals(fm1) ) ), 'pnorm' )


## ANOVA table
aov.fm1 <- aov( Evaluation ~ Error( Officer ), data=apex )
summary( aov.fm1 )