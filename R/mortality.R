library(boot)
mortality = read.csv('./data/mortality.csv')

## Outcome model
prob.Z0 = ( mortality[1,4]+mortality[4,4])/(( mortality[1,4]+ mortality[2,4]+mortality[4,4] + mortality[5,4]) )
prob.Z1 = 1 - prob.Z0

## Estimator of the causal effect
E.Y0 = mortality[4,5] * prob.Z0 + mortality[5,5] * prob.Z1
E.Y1 = mortality[1,5] * prob.Z0 + mortality[2,5] * prob.Z1

## ATT 
prob.Z1.X1 = mortality[2,4]/(mortality[1,4]+mortality[2,4])
prob.Z0.X1 = mortality[1,4]/( mortality[1,4]+ mortality[2,4]) 

E.Y0.X1 = mortality[4,5] * prob.Z0.X1 + mortality[5,5] * prob.Z1.X1

## Exposure mode

## Compute the proportions who died
mortdat=NULL
mortdat$Z <- c(0,0,0,0,1,1,1,1)
mortdat$X <- c(0,0,1,1,0,0,1,1)
mortdat$Y <- c(0,1,0,1,0,1,0,1)
mortdat$n <- c((mortality[4,4]-mortality[4,3]), mortality[4,3], (mortality[1,4]-mortality[1,3]), 
               mortality[1,3], (mortality[5,4]-mortality[5,3]), mortality[5,3], ( mortality[2,4]-mortality[2,3]),mortality[2,3])

## Compute the proportions who died
mortdat$p = mortdat$n/sum(mortdat$n)

## Compute e(Z=0)
eZ0 = sum( mortdat$n[3:4])/sum( mortdat$n[1:4])
## Compute e(Z=1)
eZ1 = sum( mortdat$n[7:8])/sum( mortdat$n[5:8])

mortdat$eZ = eZ0 * (1-mortdat$Z) + eZ1 * mortdat$Z

## Compute the summands of the estimating equation 
mortdat$s1 = mortdat$X * mortdat$Y/mortdat$eZ
mortdat$s0 = (1-mortdat$X) * mortdat$Y/(1-mortdat$eZ)

## Estiamte the expected values of the potential outcomes
E.Y1 = sum( mortdat$s1 *mortdat$p )
E.Y0 = sum( mortdat$s0 * mortdat$p)

mortdat = data.frame(mortdat)

list(E.Y1=E.Y1,E.Y0=E.Y0, mortdat=mortdat)

## ATT
e0 = sum( mortdat$X * mortdat$p)
s = mortdat$Y * (1-mortdat$X) * mortdat$eZ/( e0 * (1-mortdat$eZ))

E.Y0.T1 = sum( s*mortdat$p)
E.Y0.T1


## Exposure modeling

## Estimate P(T=1)
e0 <- sum( mortdat$X * mortdat$p )
## Compute the summands of the esimating equation
s <- mortdat$Y * (1-mortdat$X) * mortdat$eZ / (e0 * (1-mortdat$eZ) )
## Estimate E(Y0|Y1)
E.exp.Y0.T1 <- sum( s*mortdat$p)
