bp <- read.table("data/3502/bloodpress.txt", header=TRUE)

cor( cbind( bp$Age, bp$Weight, bp$BSA, bp$Dur, bp$Pulse, bp$Stress) )
pairs(~Age+Weight+BSA+Dur+Pulse+Stress, data=bp)


full.model <- lm( BP~.-Pt, data=bp)
summary(full.model)

## BSA Pulse, Weight seems correlated.
lm.bsa.only <- lm(BP~BSA, data=bp)
summary(lm.bsa.only)

lm.pulse.only <- lm(BP~Pulse, data=bp)
summary( lm.pulse.only )

## Calculate the variance inflation factor (VIF)
## install.packages("HH")
## Manually calculate VIF for Weight

lm.weight <- lm( Weight ~ .-Pt-BP-Weight, data=bp)
VIF.weight <- 1/(1- summary( lm.weight)$r.squared )
library(HH)
vif( full.model )


## Compare the full model with the model without Pulse by using within sample prediction
full.model <- lm(BP~.-Pt, data=bp)
reduced.model <- lm(BP~.-Pt-Pulse, data=bp)

summary(full.model)
summary(reduced.model)

ols_mallows_cp( reduced.model, full.model) 
ols_mallows_cp( full.model, full.model) 

sum( (bp[,2] - full.model$fitted.values)^2 )
sum( (bp[,2] - reduced.model$fitted.values)^2 )

## Compare the full model with the model without Pulse by using leave-one-out prediction

pred.full <- rep( 0, 20 )
pred.reduced <- rep( 0, 20 )

for(i in 1:20)
  {
    cv.full <- lm( BP ~ .-Pt, data= bp[ setdiff(c(1:20), i), ] )
    pred.full[i] <- predict( cv.full, bp[i, ])

    cv.reduced <- lm( BP ~ .-Pt-Pulse, data= bp[ setdiff(c(1:20), i), ] )
    pred.reduced[i] <- predict( cv.reduced, bp[i, ])

  }

sum( ( bp[,2]-pred.full)^2 )
sum( ( bp[,2] - pred.reduced)^2 )



## Compare six models
reduced.model.1 <- lm( BP ~ .-Pt - Age, data=bp)
reduced.model.2 <- lm( BP ~ .-Pt - Weight, data=bp)
reduced.model.3 <- lm( BP ~ .-Pt - BSA, data=bp)
reduced.model.4 <- lm( BP ~ .-Pt - Dur, data=bp)
reduced.model.5 <- lm( BP ~ .-Pt - Pulse, data=bp)
reduced.model.6 <- lm( BP ~ .-Pt - Stress, data=bp)

aic.score <- c( AIC( reduced.model.1),  AIC( reduced.model.2),  AIC( reduced.model.3),  AIC( reduced.model.4),  AIC( reduced.model.5),  AIC( reduced.model.6) )
bic.score <- c( BIC( reduced.model.1),  BIC( reduced.model.2),  BIC( reduced.model.3),  BIC( reduced.model.4),  BIC( reduced.model.5),  BIC( reduced.model.6) )


BIC(full.model)
AIC(full.model, k=log(20) )



## Forward regression
step( lm(BP~1, data=bp), scope=list(lower=~1, upper=~Age+Weight+BSA+Dur+Pulse+Stress), direction="forward")

## Backward regression
step( lm(BP~Age +Weight+BSA+Dur+Pulse+Stress, data=bp), direction="backward")

## step regression
step( lm(BP~1, data=bp), scope=list(lower=~1, upper=~Age+Weight+BSA+Dur+Pulse+Stress), direction="both")

## Lasso
library(lars)
x <- data.matrix( bp[,3:8] )
y <- data.matrix( bp[,2] )
lasso.fit <- lars(x=x, y=y)
plot( lasso.fit )
