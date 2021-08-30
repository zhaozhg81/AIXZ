## Demonstrate bias

sample.size = 50
numSim = 1000
prob = 0.3

X1.bar = array(0, numSim)

for( numsim in 1:numSim)
  {
    X = rbinom( sample.size, 1, prob)
    X1.bar[numsim] = mean(X)
  }

bias1 = mean(X1.bar) - prob
variance1 = var(X1.bar)



sample.size = 10000
numSim = 1000
prob = 0.3

X2.bar = array(0, numSim)

for( numsim in 1:numSim)
  {
    X = rbinom( sample.size, 1, prob)
    X2.bar[numsim] = mean(X)
  }

bias2 = mean(X2.bar) - prob
variance2 = var(X2.bar)




## Demonstrate consistency

sample.size = c(1:100)*10

prob=0.3
X.bar = array(0, 100)

for(i in 1:length( sample.size) )
{
  n = sample.size[i]
  X = rbinom(n, 1, prob)
  X.bar[i] = mean(X) 
}

plot( sample.size, X.bar, 'l', col='red', xlab="sample size", ylab="sample mean", ylim=c(0.1,0.5) )
points( sample.size, X.bar *0 + prob, 'l', col='black')
legend(x=200, y=0.2, c("sample mean","true parameter"), col=c("red","black"), lty=c(1,1) )


## Compare the probability that |hat{\theta}-\theta|>epsilon
epsilons=c( 0.05, 0.03, 0.01, 0.005 )

sample.size = c(1:50)*100
REP = 1000
prob.object = array(0, c( length(sample.size), 4) )

for( i in 1:length(sample.size) )
{
  n = sample.size[i]
  X = array( rbinom(n*REP, 1, prob), c(n, REP ) )
  X.bar = apply(X, 2, mean) 
  prob.object[i, 1] = mean( abs(X.bar - prob) > epsilons[1] )
  prob.object[i, 2] = mean( abs(X.bar - prob) > epsilons[2] )
  
  prob.object[i, 3] = mean( abs(X.bar - prob) > epsilons[3] )
  prob.object[i, 4] = mean( abs(X.bar - prob) > epsilons[4] )
}

plot( sample.size, prob.object[,4], 'l', col='red', xlab="sample size", ylab="prob", ylim=c(0,1) )
points( sample.size, prob.object[,3], 'l', col='green' )
points( sample.size, prob.object[,2], 'l', col='blue' )
points( sample.size, prob.object[,1], 'l', col='black' )
legend(x=2000,y=1.0, c("epsilon=0.05","epsilon=0.03","epsilon=0.01","epsilon=0.005"), col=c("red","green","blue","black"), lty=c(1,1,1,1) )


