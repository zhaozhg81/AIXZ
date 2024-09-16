## The common population is N(mu, 1)
mu = 1.75
sigma=0.2

## Bias for a fixed sample size
n = 100
num_rep_sampling = 1000
sample_mean_full = rep(0, num_rep_sampling)
sample_mean_partial=rep(0, num_rep_sampling)
sample_variance = rep(0, num_rep_sampling)

for( i in 1:num_rep_sampling)
{
  X = rnorm(n, mu, sigma)
  sample_mean_full[i] = mean(X)
  sample_mean_partial[i] = mean( X[1:10]) 
  sample_variance[i] = var( X )
}

mean( sample_mean_full )
mean( sample_variance )

## Variance
var( sample_mean_full )
var( sample_mean_partial )


## Consistency
ns = 100*c(1:30)
sample_mean_all = array(0, c(length(ns), num_rep_sampling) )
for( i in 1:30)
  {
    X = array( rnorm( ns[i]*num_rep_sampling, mu, sigma), c( ns[i], num_rep_sampling) )
    sample_mean_all[i,] = apply(X, 2, mean)
    print(i)
}

apply( abs( sample_mean_all - mu ) > 0.05, 1, mean) 
