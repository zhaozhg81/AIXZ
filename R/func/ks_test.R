library(kolmim)

ks_test <- function( data, distribution, ...)
  {
    ## data: a vector of the data set
    ## Distribution: the name of the distribution
    ## ## Support these distribution:
    ## ## ## 'pnorm', N(mu, sigma)
    ## ## ## 'punif', U(min, max)
    ## ## ## 'pgamma',Gamma(shape, scale)
    ## ## ## 'pt', T_df
    ##
    ## ...: additional parameter for the distribution
    ## ## (a) 'pnorm', mu, sigma (default 0, 1 )
    ## ## (b) 'punif', min, max (default value 0, 1)
    ## ## (c) 'pgamma', shape, scale (default value, 1, 1)
    ## ## (d) 'pt, df (default 10 )

    para <- unlist( list(...) )
    n <- length( data )
    data.sort <- sort(data, decreasing=FALSE )

    
    if( distribution =='pnorm')
      {
        
        cdf.norm <- pnorm( data.sort, mean=para[1], sd=para[2] )
        ks.stat <- max( abs( cdf.norm - c(0:(n-1))/n), abs( cdf.norm-c(1:n)/n) )
                  
      }else if( distribution =='punif')
        {
          
          cdf.unif <- punif( data.sort, min=para[1], max=para[2] )
          ks.stat <- max( abs( cdf.unif - c(0:(n-1))/n), abs( cdf.unif-c(1:n)/n) )
          
        }else if( distribution == 'pgamma')
          {
            
            cdf.gamma <- pgamma( data.sort, shape=para[1], scale=para[2] )
            ks.stat <- max( abs( cdf.gamma - c(0:(n-1))/n), abs( cdf.gamma-c(1:n)/n) )
            
          }else if( distribution == 'pt' )
            {

              cdf.t <- pt( data.sort, df=para[1] )
              ks.stat <- max( abs( cdf.t - c(0:(n-1))/n), abs( cdf.t-c(1:n)/n) )

            }else {
              print( "Not supported distribution.")
            }
    list( ks.stat=ks.stat, sample_size=n, distribution=distribution, para=para)
    
  }

ks_test_GMM <- function( data, K, pi, mu, sigma)
{
  ## data: a vector of the data set

  n <- length( data )
  data.sort <- sort(data, decreasing=FALSE )
  
  
  cdf.gmm <- array(0, n)
  for(i in 1:n)
  {
    cdf.gmm[i] =  sum( pi * pnorm( data.sort[i], mu, sigma, lower.tail=TRUE) )
  }
    
  ks.stat <- max( abs( cdf.gmm - c(0:(n-1))/n), abs( cdf.gmm-c(1:n)/n) )
    
  list( ks.stat=ks.stat, sample_size=n, p.value = pkolm(ks.stat, n ) )
  
}
