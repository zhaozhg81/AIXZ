n = 100
numSim = 1000
theta = 2

esti_MOM_1 = array(0, numSim)
esti_MOM_2 = array(0, numSim)
esti_MLE = array(0, numSim)

for( i in 1:numSim)
{
  X = runif(n, 0, theta)
  esti_MOM_1[i] = 2 * mean(X)
  esti_MOM_2[i] = sqrt( 2* mean(X^2) )
  esti_MLE[i] = max( X) 
  
}

MSE_MOM_1 = mean( (esti_MOM_1 - theta)^2 )
MSE_MOM_2 = mean( (esti_MOM_2 - theta)^2 )
MSE_MLE = mean(( esti_MLE - theta)^2)