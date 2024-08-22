library(MASS)
library(multcomp)

data(ships)

## Histogram of the data
## postscript("hist.eps",horizontal=FALSE)
hist( ships$incidents, breaks=20)
## dev.off()


## PMF of poisson
y=c(0:50)
## postscript("poisson_pmf.eps",horizontal=FALSE)
plot( y, dpois(y, 2.5), 'l', col='red')
points(y, dpois(y, 5), 'l', col='blue')
points(y, dpois(y, 10), 'l', col='green')
points(y, dpois(y, 30), 'l', col='purple')
legend(x=30, y=0.20, c("2.5","5","10","30"), lty=c(1,1,1,1), col=c("red","blue","green","purple") )
## dev.off()

## KS test
y <- rpois(100, 2.5)
ks.test( (y-mean(y))/sqrt(var(y)), 'pnorm')

y <- rpois(100, 30)
ks.test( (y-mean(y))/sqrt(var(y)), 'pnorm')

ks.test( (ships$incidents - mean(ships$incidents) )/sqrt( var(ships$incidents) ), 'pnorm')


## Poisson fit

library(MASS)
data(ships)
ships2 <- subset(ships, service > 0)
ships2$year <- as.factor(ships2$year)
ships2$period <- as.factor(ships2$period)


## Let us consider a log-linear model including all the variables. We
## first exclude ships with 0 months of service and convert the
## period and year variables to factors:
cargo.pois <- glm(formula = incidents ~ type + year + period,
     family = poisson(link = "log"), data = ships2,
     offset = log(service))
summary( cargo.pois )

cargo.pairwise <- glht( cargo.pois, linfct=mcp(type="Tukey") )

## Model with interaction
cargo.pois.inter <- glm(formula = incidents ~ type + year + period + type:year,
     family = poisson(link = "log"), data = ships2,
     offset = log(service))
summary( cargo.pois.inter)


## ANOVA analysis
anova(cargo.pois, cargo.pois.inter )


## anova
m0 = update( cargo.pois, .~.-type)
m1 = update( cargo.pois, .~.-year)
m2 = update( cargo.pois, .~.-period)

anova( m0, cargo.pois )
anova( m1, cargo.pois )
anova( m2, cargo.pois )
