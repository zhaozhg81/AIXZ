library(boot)

whatif = read.csv('./data/whatif.csv')

estimator = function( data, ids)
{
  dat <- data[ids,]
  coef <- glm( Y~X+A+Z, family=binomial, data=dat)$coef
  coef
  
}

boot.out = boot( data=whatif, statistic=estimator, R=1000)



## Outcome modeling
stand.r = function(data, ids)
{
  dat = data[ids,]
  ## Find the marginal expected value of Z
  E.Z = mean( dat$Z )
  ## Fit outcome model 
  beta = lm(Y~A*Z, data=dat)$coef
  ## Compute marginal expected potential outcomes
  E.Y0 = beta[1] + beta[3] * E.Z
  E.Y1 = beta[1] + beta[2] + beta[3] *E.Z + beta[4]*E.Z
  
  ## Return effect measures
  rd = E.Y1 - E.Y0
  logrr = log( E.Y1/E.Y0 )
  
  c(E.Y0, E.Y1, rd, logrr)
}

boot.outcome = boot( data=whatif, statistic=stand.r, R=1000)


## ATT for the outcome model
standatt.r = function(data, ids)
{
  dat = data[ids,]
  ## Computethe expected value of H given A=1
  E.HA = mean( dat$Z[ dat$A==1])
  ## Fit the outcome model
  beta = lm(Y~A*Z, data=dat)$coef
  ## Compute the expected potential outcomes given A=1
  E.Y0.A = beta[1]+ beta[3] * E.HA
  E.Y1.A = beta[1] + beta[2] + beta[3] * E.HA + beta[4] * E.HA
  rd = E.Y1.A - E.Y0.A
  logrr = log( E.Y1.A/E.Y0.A)
  
  c(E.Y0.A, E.Y1.A, rd, logrr)
  
}


boot.att = boot( data=whatif, statistic=standatt.r, R=1000)