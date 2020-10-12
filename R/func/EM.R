

EM <- function(X, K=2, pi.ini=c(0.5,0.5), mu.ini=c(-1,1), sigma.ini=c(1,1), DELTA=0.001, verbose=FALSE )
{
  ## When the data follows a mixture of Gaussian distribution. The parameters could be estimated
  ## using the EM algorithm.
  ## 
  ## This function calculate the estimated parameters using EM algorithm. 
  ## The default choice of the number of components is set as 2. 
  
  n = length(X)
  
  delta=1
  pi.old= pi.ini
  mu.old = mu.ini
  sigma.old= sigma.ini
  
  pi.new = pi.ini
  mu.new = mu.ini
  sigma.new = sigma.ini
  
  l.fdr = array(0, c(n, K ) )
  x.expand = array(X, c(n,1)) %x% array(1, c(1, K ) )
  
  while( delta > DELTA )
  {
    for(k in 1:K)
    {
      l.fdr[,k] = pi.old[k]* dnorm(X, mu.old[k], sigma.old[k] )
    }
    colsum  = apply( l.fdr, 1, sum )
    for(k in 1:K)
    {
      l.fdr[,k] = l.fdr[,k]/colsum
      mu.new[k] = sum( l.fdr[,k] * X )/sum( l.fdr[,k])
      sigma.new[k] = sqrt( sum( (X-mu.new[k])^2*l.fdr[,k] )/sum( l.fdr[,k]) )
    }
    pi.new = apply( l.fdr, 2, sum)/n
    
    delta= sum( (pi.new-pi.old)^2 ) + sum( (mu.new-mu.old)^2 ) + sum( ( sigma.new-sigma.old)^2 )
    
    
    if(verbose==TRUE)
      print( paste("Delta = ", delta ) )
    
    pi.old=pi.new
    mu.old=mu.new
    sigma.old=sigma.new 
    
  }
  
  ## Return the estimated value
  list(K=K, n=n, pi.esti=pi.new, mu.esti=mu.new, sigma.esti=sigma.new)
  
}