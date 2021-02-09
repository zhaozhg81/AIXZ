nncopy <- function( vector, num, dim)
{
  n = length(vector)
  if( dim==1 )
  {
    nnvector = matrix( vector, c(1,n) ) %x% matrix(rep(1,num), c(num,1) )
  }
  if( dim==2 )
  {
    nnvector = matrix(rep(1,num), c(1, num)) %x% matrix( vector, c(n,1) )
  } 
  
  nnvector
}