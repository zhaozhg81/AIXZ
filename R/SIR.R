library(LassoSIR)
library(glmnet)
library(dr)
data(ais)

s0 <- dr(LBM~log(SSF)+log(Wt)+log(Hg)+log(Ht)+log(WCC)+log(RCC)+
                 log(Hc)+log(Ferr),data=ais,slice.function=dr.slices.arc,nslices=8,
                 chi2approx="wood",numdir=4,method="sir")

s2 <- update(s0,method="save")


set.seed(2)
Y=ais$LBM
X=cbind(log(ais$SSF), log(ais$Wt), log(ais$Hg), log(ais$Ht), log(ais$WCC), log(ais$RCC), log(ais$Hc), log(ais$Ferr) )

lassosir <- LassoSIR(X=X, Y=Y, H=8, screening=FALSE )
lassosir$beta = lassosir$beta/sqrt( sum( lassosir$beta^2 ) )

predX = X%*% lassosir$beta 
plot(predX, Y)


