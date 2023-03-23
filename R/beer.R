if (!requireNamespace("pacman")){
  install.packages("pacman")
}
pacman::p_load(sjstats)
pacman::p_load(sjPlot)
pacman::p_load(lme4)
pacman::p_load(MASS)

library(nlme)

Sodium <- c(24.4,22.6,23.8,22.0,24.5,22.3,25.0,24.5,
    10.2,12.1,10.3,10.2,9.9,11.2,12.0,9.5,
    19.2,19.4,19.8,19.0,19.6,18.3,20.0,19.4,
    17.4,18.1,16.7,18.3,17.6,17.5,18.0,16.4,
    13.4,15.0,14.1,13.1,14.9,15.0,13.4,14.8,
    21.3,20.2,20.7,20.8,20.1,18.8,21.1,20.3)
Brand <- factor(paste("Brand",rep(1:6,rep(8,6)), sep=""))
Bottle <- rep(1:8,6)
beer <- data.frame(Sodium,Brand,Bottle)

## post.script("./figure/beer.eps",horizontal=FALSE)
par(mar = c(4.5,4.5,1,3.5))
boxplot(Sodium ~ Brand, data = beer, xlab = "Brand", ylab = "Sodium")
## dev.off()

fm3 <- lme(Sodium ~ 1, data = beer, random = ~ 1|Brand)

## Model diagnosis
## Residual plot
plot( fitted( fm3 ), residuals( fm3 ) )
## KS test
ks.test( residuals(fm3)/ sqrt( var( residuals(fm3) ) ), 'pnorm' )
## ANOVA table

aov.fm3 <- aov( Sodium ~ Error( Brand ), data=beer )
summary( aov.fm3 )

F.stat <- 170.9/0.716

p.value <- 1- pf( F.stat, 5, 42 )

sjPlot::tab_model( fm3 )

