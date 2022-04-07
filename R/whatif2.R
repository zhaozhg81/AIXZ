library(boot)

whatif2 = read.csv('./data/whatif2.csv')

standout.r = function( data=whatif2, ids=c(1:nrow(whatif2)))
{
  dat = data[ids, ]
  
  ## Fit the parametric outcome model
  lmod = glm(vl4~ A + lvlcont0, family=binomial, data=dat)
  dat0 <- dat1 <- dat
  
  ## Create a data set with everyone untreated
  dat0$A= 0
  ## Create a dataset with everyone treated
  dat1$A= 1
  ## Compute the expected potential outcome for each participant if untreated
  E.Y.hat0 = predict(lmod, newdata=dat0, type="response")
  ## Compute the expected potential outcome for each participant if treated
  E.Y.hat1 = predict(lmod, newdata=dat1, type="response")
  ## Estimate the average potential outcomes
  E.Y0 = mean(E.Y.hat0)
  E.Y1 = mean(E.Y.hat1)
  
  ## Estimate the effect measures
  rd = E.Y1 - E.Y0
  logrr  = log(E.Y1/E.Y0)
  c(E.Y0,E.Y1, rd, logrr)
  
}

boot.out = boot( data=whatif2, statistic= standout.r, R=1000)


## exposure model
standexp.r = function(data,ids)
{
  dat = data[ids,]
  ## Estimate the parametric exposure model
  e = fitted( glm(A~lvlcont0, family=binomial, data=dat) )
  ## Compute the weights
  dat$W = (1/e)*dat$A + (1/(1-e))*(1-dat$A)
  ## Fit the weighted linear model
  beta  = glm( vl4~A, data=dat, weights=W)$coef
  ## Estimate the expected potential outcomes
  E.Y0 = beta[1]
  E.Y1 = beta[1] + beta[2]
  
  ## Estimate the effect measures
  rd = E.Y1-E.Y0
  rr = log( E.Y1/E.Y0)
  
  c(E.Y0,E.Y1, rd, rr)
}

boot.exp = boot( data=whatif2, statistic= standexp.r, R=1000)

## Double robust standardization
badstanddr.r = function(data, ids)
{
  dat = data[ids,]
  ## Fit the parametric exposure model
  e = fitted( glm(A~lvlcont0, family=binomial, data=dat))
  ## Fit a nonparametric outcome model that we do not believe 
  lmod = glm(vl4~A, family=binomial, data=dat)
  dat0 <- dat1 <- dat
  dat0$A =0
  dat1$A = 1
  ## Predict the potential outcomes for each participant
  E.Y.hat0 = predict( lmod, newdata=dat0, type="response")
  E.Y.hat1 = predict( lmod, newdata=dat1, type="response")
  ## Use the DR estimating equation to estimate expected potential outcomes
  E.Y0 = mean( dat$vl4 *(1-dat$A)/(1-e) + E.Y.hat0 * (e-dat$A)/(1-e)  )
  E.Y1 = mean( dat$vl4 *(dat$A/e) -E.Y.hat1 * (dat$A-e)/e )
  
  rd = E.Y1-E.Y0
  rr = log(E.Y1/E.Y0)
  
  c(E.Y0, E.Y1, rd,rr)
}

boot.doublerobust = boot(data=whatif2, statistic= badstanddr.r, R=1000)

boot.doublerobust$t0-1.96*apply( boot.doublerobust$t,2,sd)
boot.doublerobust$t0+1.96*apply( boot.doublerobust$t,2,sd)
