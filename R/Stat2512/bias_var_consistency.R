## The common population is N(mu, 1)
mu = 1

## Bias for a fixed sample size
n = 100
num_rep_sampling = 1000
sample_mean_full = rep(0, num_rep_sampling)
sample_mean_partial=rep(0, num_rep_sampling)

for( i in 1:num_rep_sampling)
{
  X = rnorm(n, mu, 1)
  sample_mean_full[i] = mean(X)
  sample_mean_partial[i] = mean( X[1:10]) 
}

## Variance
mean( sample_mean_full)
mean( sample_mean_partial)

var( sample_mean_full )
var( sample_mean_partial )


## Consistency
ns = c(2:40)^2
sample_mean_all = array(0, c(length(ns), num_rep_sampling) )
for( i in 1:length(ns))
  {
    X = array( rnorm( ns[i]*num_rep_sampling, mu, 1), c( ns[i], num_rep_sampling) )
    sample_mean_all[i,] = apply(X, 2, mean)
    print(i)
}

apply( abs( sample_mean_all - mu ) > 0.05, 1, mean) 

plot(ns, sample_mean_all[,1], 'l', ylim=c(0.6,1.4))
for(i in 2:100)
  points(ns, sample_mean_all[,i], 'l')
points(ns, array(mu, length(ns)), 'l', col='red', lwd=5)
