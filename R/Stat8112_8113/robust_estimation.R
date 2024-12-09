### The variables are 
## state id (sid), 
## state name (state), 
## violent crimes per 100,000 people (crime), 
## murders per 1,000,000 (murder), 
## the percent of the population living in metropolitan areas (pctmetro), 
## the percent of the population that is white (pctwhite), 
## percent of population with a high school education or above (pcths), 
## percent of population living under poverty line (poverty) 
## percent of population that are single parents (single). 
## 
## It has 51 observations. We are going to use poverty and single to predict crime.

library(foreign)
library(MASS)

cdata <- read.dta("https://stats.idre.ucla.edu/stat/data/crime.dta")
summary(cdata)

head(cdata)
summary(ols <- lm(crime ~ poverty + single, data = cdata))
plot(ols, las = 1)

## 
cdata[c(9, 25, 51), 1:2]
d1 <- cooks.distance(ols)
r <- stdres(ols)
a <- cbind(cdata, d1, r)
a[d1 > 4/51, ]

## 
rabs <- abs(r)
a <- cbind(cdata, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]


## Robust estimation, Huber weight
rr.huber <- rlm(crime ~ poverty + single, data = cdata)
summary( rr.huber)
hweights <- data.frame(state = cdata$state, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15, ]

## Biweights
rr.bisquare <- rlm(crime ~ poverty + single, data=cdata, psi = psi.bisquare)
summary(rr.bisquare)
biweights <- data.frame(state = cdata$state, resid = rr.bisquare$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]

