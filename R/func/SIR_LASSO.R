
CUTOFF <- function(Y, d, m1, n, H1 )
  {
    Z <- matrix( rnorm(n*d,0,1), n, d)
    Z.sliced.variance <- SVS( Z, Y, d, m1, n, H1 )
    cutoff <- max( Z.sliced.variance )
    cutoff
  }

SVS <- function( X, Y, p, m, n, H ){

  ## p <- 1200
  ## n <- 480
  ## H <- 10
  ## m <- n/H

  sliced.variance <- rep(0, p)
  x.sliced.mean <- matrix(0, nrow=H, ncol= p )
  x.sliced.mean
  ## Order the observation
  ORD <- order( Y )
  for( h in 1:H )
    {
      x.sliced <- X[ ORD[ (1+(h-1)*m):(h*m) ], ]
      x.sliced.mean[h, ] <- apply( x.sliced, 2, mean )
    }
  sliced.variance <- apply( x.sliced.mean, 2, var )

}

SIR <- function( X, Y, p, m, n, H, numdir){

  ## p <- 1200
  ## n <- 480
  ## H <- 10
  ## m <- n/H

  x.sliced.mean <- matrix(0, nrow=H, ncol= dim(X)[2] )
  x.sliced.mean
  ## Order the observation
  ORD <- order( Y )
  grand.mean <- apply(X, 2, mean )

  for( h in 1:H )
    {
      x.sliced <- X[ ORD[ (1+(h-1)*m):(h*m) ], ]
      x.sliced.mean[h, ] <- apply( x.sliced, 2, mean ) - grand.mean
    }
  ## Estimator of the matrix lambda_p
  LambdaHat <- t(x.sliced.mean) %*% x.sliced.mean/H
  sdr <- eigen(LambdaHat)$vectors[, c(1:numdir) ]

  sdr
}

SSIR <- function(X, SigmaX.inv, Y, p, m1, m2, n, H1, H2, no.dim, categorical=FALSE){
  if(categorical==FALSE)
    {
      sliced.variance <- SVS( X, Y, p, m1, n, H1 )
      thresh <- CUTOFF( Y, p, m1, n, H1 )
      ORDER.svs <- order( sliced.variance, decreasing=TRUE )
      
      if( sum( sliced.variance>=thresh) <2 ){
        keep.ind <- ORDER.svs[1:2]
      }else{
        keep.ind <- which(sliced.variance>=thresh)
      }
      
      ## keep.ind <- find.elbow( sliced.variance, p )
      
      ## Apply the sir method
      no.dim <- min( no.dim, length(keep.ind) )
      sdr <- SIR( X[,keep.ind], Y, p, m2, n, H2, numdir= no.dim )
      
      eta <- array(0, c(p, no.dim ) )
      eta[ keep.ind, 1:no.dim] <- sdr
      
      beta <- SigmaX.inv %*% eta
    }else{
      Y.unique <- unique(Y)
      H <- length( Y.unique )
      ORD <- which( Y==Y.unique[1] )
      
      nH <- sum( Y == Y.unique[1] )
      for( i in 2:H )
        {
          ORD <- c( ORD, which(Y==Y.unique[i]) )
          nH <- c(nH, sum( Y==Y.unique[i] ) )
        }
      X <- X[ORD, ]
      
      M <- matrix( 0,  nrow=H, ncol=n )
      M[ 1, 1:nH[1] ] <- 1/nH[1]
      for(i in 2:H)
        M[ i, (sum(nH[1:(i-1)])+1):sum(nH[1:i]) ] <- 1/nH[i]
      x.sliced.mean <- M%*%X
      sliced.variance <- apply( x.sliced.mean, 2, var )
      keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
      X <- X[, keep.ind]
      ## Apply the sir method
      X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
      grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
      X.stand.ord <- X - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
      X.H <- M%*% X.stand.ord

      no.dim <- min( no.dim, length(keep.ind) )
      
      LambdaHat <- t(X.H) %*% X.H/H
      sdr <- eigen(LambdaHat)$vectors[, c(1:no.dim) ]
            
      eta <- array(0, c(p, no.dim ) )
      eta[ keep.ind, 1:no.dim] <- sdr
      
      beta <- SigmaX.inv %*% eta
    }
  
  list(beta=beta, no.keep=length(keep.ind) )
}


## SIR_LASSO.A2.data <- function( X, Y, p, n, H, m, no.dim, solution.path=FALSE, categorical=FALSE, nfolds=10)
##   {
##     beta.hat <- array(0, c(p, no.dim) )
##     Y.tilde <- array(0, c(n, no.dim) )

##     if( categorical==FALSE)
##       {
##         sliced.variance <- SVS( X, Y, p, m1, n, H1 )
##         ## keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:floor(sqrt(n))] )
##         keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
        
        
##         X <- X[, keep.ind]
        
##         X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
##         ORD <- order( Y )
##         grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
##         X.stand.ord <- X[ ORD, ] - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
    
        
##         ## for( h in 1:H )
##         ##   {
##         ##     x.sliced <- X.stand.ord[ (1+(h-1)*m):(h*m) , ]
##         ##     X.H[h, ] <- apply( x.sliced, 2, mean )
##         ##   }
        
##         ## Another way to calculate x.sliced.mean X_H. Note that X_H can be written as MX where M is a block diagonal matrix.
##         M <- diag( H ) %x% matrix( 1, nrow=1,ncol= m)/m
##         X.H <- M%*% X.stand.ord
##       }else{
##         Y.unique <- unique(Y)
##         H <- length( Y.unique )
##         ORD <- which( Y==Y.unique[1] )
        
##         nH <- sum( Y == Y.unique[1] )
##         for( i in 2:H )
##           {
##             ORD <- c( ORD, which(Y==Y.unique[i]) )
##             nH <- c(nH, sum( Y==Y.unique[i] ) )
##           }
##         X <- X[ORD, ]

##         M <- matrix( 0,  nrow=H, ncol=n )
##         M[ 1, 1:nH[1] ] <- 1/nH[1]
##         for(i in 2:H)
##           M[ i, (sum(nH[1:(i-1)])+1):sum(nH[1:i]) ] <- 1/nH[i]
##         x.sliced.mean <- M%*%X
##         sliced.variance <- apply( x.sliced.mean, 2, var )
##         keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
##         X <- X[, keep.ind]
        
##         X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
##         grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
##         X.stand.ord <- X - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
##         X.H <- M%*% X.stand.ord

##       }
    
##     LambdaHat <- t(X.H) %*% X.H/H
##     temp <- eigen( LambdaHat )
##     res.eigen.value <- temp$values
##     for( ii in 1:no.dim)
##       {
##         eigen.vec <- temp$vectors[, ii ]
       
##         Y.tilde[,ii] <- t(M) %*% M %*% X.stand.ord %*% eigen.vec/( m* temp$values[ii] )
##       }
##     if( solution.path==FALSE )
##       {
##         if(no.dim==1){
##           lars.fit.cv <- cv.glmnet( X.stand.ord, Y.tilde, nfolds=nfolds )
          
##           ind <- max( which( lars.fit.cv$cvm==min(lars.fit.cv$cvm) ) )
##           if(ind==1)
##             ind <- 2
##           ## two ways of choosing the threshold. 1. choose the one with the smallest cvm; 2. choose the one with a significant smallest cvm. Namely, choose the one such that the lower bound is higher than the upper bound of the one with the smallest cvm.
          
##           ## lambda <- lars.fit.cv$lambda[ max( ( lars.fit.cv$cvlo[1:ind] >= lars.fit.cv$cvup[ind] ) * c(1:ind) ) ]
##           lambda <- lars.fit.cv$lambda[ind]
##           lars.fit <- glmnet(X.stand.ord, Y.tilde, lambda=lambda)
##           beta.hat[ keep.ind ] <- as.double( lars.fit$beta )
##         }else{
##           for(ii in 1:no.dim)
##             {
##               lars.fit.cv <- cv.glmnet( X.stand.ord, Y.tilde[,ii], nfolds=nfolds )
              
##               ind <- max( which( lars.fit.cv$cvm==min(lars.fit.cv$cvm) ) )
##               if(ind==1)
##                 ind <- 2
##               ## two ways of choosing the threshold. 1. choose the one with the smallest cvm; 2. choose the one with a significant smallest cvm. Namely, choose the one such that the lower bound is higher than the upper bound of the one with the smallest cvm.
              
##               ## lambda <- lars.fit.cv$lambda[ max( ( lars.fit.cv$cvlo[1:ind] >= lars.fit.cv$cvup[ind] ) * c(1:ind) ) ]
##               lambda <- lars.fit.cv$lambda[ind]
##               lars.fit <- glmnet(X.stand.ord, Y.tilde[,ii], lambda=lambda)
##               beta.hat[ keep.ind, ii ] <- as.double( lars.fit$beta )
##             }
##         }
##         list( beta= beta.hat, eigen.value=res.eigen.value)
##       }else{
##         lars.fit.all <- list()
##         for(ii in 1:no.dim)
##           {
##             lars.fit.all[[ii]] <- glmnet( X.stand.ord, Y.tilde[,ii] )
##           }
##         lars.fit.all
##       }
##   }



## SIR_LASSO.A2 <- function( X, Y, p, n, H, m, no.dim, solution.path=FALSE, categorical=FALSE, nfolds=10)
##   {
##     beta.hat <- array(0, c(p, no.dim) )
##     Y.tilde <- array(0, c(n, no.dim) )

##     if( categorical==FALSE)
##       {
##         sliced.variance <- SVS( X, Y, p, m1, n, H1 )
##         ## keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:floor(sqrt(n))] )
##         keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
        
        
##         X <- X[, keep.ind]
        
##         X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
##         ORD <- order( Y )
##         grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
##         X.stand.ord <- X[ ORD, ] - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
    
        
##         ## for( h in 1:H )
##         ##   {
##         ##     x.sliced <- X.stand.ord[ (1+(h-1)*m):(h*m) , ]
##         ##     X.H[h, ] <- apply( x.sliced, 2, mean )
##         ##   }
        
##         ## Another way to calculate x.sliced.mean X_H. Note that X_H can be written as MX where M is a block diagonal matrix.
##         M <- diag( H ) %x% matrix( 1, nrow=1,ncol= m)/m
##         X.H <- M%*% X.stand.ord
##       }else{
##         Y.unique <- unique(Y)
##         H <- length( Y.unique )
##         ORD <- which( Y==Y.unique[1] )
        
##         nH <- sum( Y == Y.unique[1] )
##         for( i in 2:H )
##           {
##             ORD <- c( ORD, which(Y==Y.unique[i]) )
##             nH <- c(nH, sum( Y==Y.unique[i] ) )
##           }
##         X <- X[ORD, ]

##         M <- matrix( 0,  nrow=H, ncol=n )
##         M[ 1, 1:nH[1] ] <- 1/nH[1]
##         for(i in 2:H)
##           M[ i, (sum(nH[1:(i-1)])+1):sum(nH[1:i]) ] <- 1/nH[i]
##         x.sliced.mean <- M%*%X
##         sliced.variance <- apply( x.sliced.mean, 2, var )
##         keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
##         X <- X[, keep.ind]
        
##         X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
##         grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
##         X.stand.ord <- X - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
##         X.H <- M%*% X.stand.ord

##       }
    
##     LambdaHat <- t(X.H) %*% X.H/H
##     temp <- eigen( LambdaHat )
##     res.eigen.value <- temp$values
##     for( ii in 1:no.dim)
##       {
##         eigen.vec <- temp$vectors[, ii ]
       
##         Y.tilde[,ii] <- t(M) %*% M %*% X.stand.ord %*% eigen.vec/( m* temp$values[ii] )
##       }
##     if( solution.path==FALSE )
##       {
##         if(no.dim==1){
##           lars.fit.cv <- cv.glmnet( X.stand.ord, Y.tilde, nfolds=nfolds )
          
##           ind <- max( which( lars.fit.cv$cvm==min(lars.fit.cv$cvm) ) )
##           if(ind==1)
##             ind <- 2
##           ## two ways of choosing the threshold. 1. choose the one with the smallest cvm; 2. choose the one with a significant smallest cvm. Namely, choose the one such that the lower bound is higher than the upper bound of the one with the smallest cvm.
          
##           ## lambda <- lars.fit.cv$lambda[ max( ( lars.fit.cv$cvlo[1:ind] >= lars.fit.cv$cvup[ind] ) * c(1:ind) ) ]
##           lambda <- lars.fit.cv$lambda[ind]
##           lars.fit <- glmnet(X.stand.ord, Y.tilde, lambda=lambda)
##           beta.hat[ keep.ind ] <- as.double( lars.fit$beta )
##         }else{
##           for(ii in 1:no.dim)
##             {
##               lars.fit.cv <- cv.glmnet( X.stand.ord, Y.tilde[,ii], nfolds=nfolds )
              
##               ind <- max( which( lars.fit.cv$cvm==min(lars.fit.cv$cvm) ) )
##               if(ind==1)
##                 ind <- 2
##               ## two ways of choosing the threshold. 1. choose the one with the smallest cvm; 2. choose the one with a significant smallest cvm. Namely, choose the one such that the lower bound is higher than the upper bound of the one with the smallest cvm.
              
##               ## lambda <- lars.fit.cv$lambda[ max( ( lars.fit.cv$cvlo[1:ind] >= lars.fit.cv$cvup[ind] ) * c(1:ind) ) ]
##               lambda <- lars.fit.cv$lambda[ind]
##               lars.fit <- glmnet(X.stand.ord, Y.tilde[,ii], lambda=lambda)
##               beta.hat[ keep.ind, ii ] <- as.double( lars.fit$beta )
##             }
##         }
##         beta.hat
##       }else{
##         lars.fit.all <- list()
##         for(ii in 1:no.dim)
##           {
##             lars.fit.all[[ii]] <- glmnet( X.stand.ord, Y.tilde[,ii] )
##           }
##         lars.fit.all
##       }
##   }



## SIR_LASSO.A3 <- function( X, Y, p, n, H, m, no.dim, categorical=FALSE, nfolds=10)
##   {

##     beta.hat <- array(0, c(p, no.dim) )
##     Y.tilde <- array(0, c(n, no.dim) )
    
##     if( categorical==FALSE)
##       {
##         sliced.variance <- SVS( X, Y, p, m1, n, H1 )
##         ## keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:floor(sqrt(n))] )
##         keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
        
        
##         X <- X[, keep.ind]
        
##         X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
##         ORD <- order( Y )
##         grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
##         X.stand.ord <- X[ ORD, ] - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
    
        
##         ## for( h in 1:H )
##         ##   {
##         ##     x.sliced <- X.stand.ord[ (1+(h-1)*m):(h*m) , ]
##         ##     X.H[h, ] <- apply( x.sliced, 2, mean )
##         ##   }
        
##         ## Another way to calculate x.sliced.mean X_H. Note that X_H can be written as MX where M is a block diagonal matrix.
##         M <- diag( H ) %x% matrix( 1, nrow=1,ncol= m)/m
##         X.H <- M%*% X.stand.ord
##       }else{
##         Y.unique <- unique(Y)
##         H <- length( Y.unique )
##         ORD <- which( Y==Y.unique[1] )
        
##         nH <- sum( Y == Y.unique[1] )
##         for( i in 2:H )
##           {
##             ORD <- c( ORD, which(Y==Y.unique[i]) )
##             nH <- c(nH, sum( Y==Y.unique[i] ) )
##           }
##         X <- X[ORD, ]
        
##         M <- matrix( 0,  nrow=H, ncol=n )
##         M[ 1, 1:nH[1] ] <- 1/nH[1]
##         for(i in 2:H)
##           M[ i, (sum(nH[1:(i-1)])+1):sum(nH[1:i]) ] <- 1/nH[i]
##         x.sliced.mean <- M%*%X
##         sliced.variance <- apply( x.sliced.mean, 2, var )
##         keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
##         X <- X[, keep.ind]
        
##         X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
##         grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
##         X.stand.ord <- X - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
##         X.H <- M%*% X.stand.ord

##       }   
    
##     LambdaHat <- t(X.H) %*% X.H/H
##     temp <- eigen( LambdaHat )
##     for( ii in 1:no.dim)
##       {
##         eigen.vec <- temp$vectors[, ii ]
       
##         Y.tilde[,ii] <- t(M) %*% M %*% X.stand.ord %*% eigen.vec/m
##       }

##     if(no.dim>1){
##       lars.fit.cv <- cv.glmnet( X.stand.ord, Y.tilde, family="mgaussian", nfolds=nfolds )
##     }else{
##       lars.fit.cv <- cv.glmnet( X.stand.ord, Y.tilde, nfolds=nfolds )
##     }
##     ind <- max( which( lars.fit.cv$cvm==min(lars.fit.cv$cvm) ) )
##     if(ind==1)
##       ind <- 2
##     ## two ways of choosing the threshold. 1. choose the one with the smallest cvm; 2. choose the one with a significant smallest cvm. Namely, choose the one such that the lower bound is higher than the upper bound of the one with the smallest cvm.
    
##     ## lambda <- lars.fit.cv$lambda[ max( ( lars.fit.cv$cvlo[1:ind] >= lars.fit.cv$cvup[ind] ) * c(1:ind) ) ]
##     lambda <- lars.fit.cv$lambda[ind]
##     if(no.dim>1){
##       lars.fit <- glmnet(X.stand.ord, Y.tilde, lambda=lambda, family="mgaussian")
##       for(ii in 1:no.dim)
##         beta.hat[keep.ind, ii] <- as.double( lars.fit$beta[[ii]] )
##     }else{
##       lars.fit <- glmnet(X.stand.ord, Y.tilde, lambda=lambda)
##       beta.hat[ keep.ind ] <- as.double( lars.fit$beta )
##     }
    
##     beta.hat
   
##   }


SIR_LASSO.2 <- function( X, Y, p, n, H, m, no.dim, categorical=FALSE,nfolds=10)
  {
    beta.hat <- array(0, c(p, no.dim) )
    
    if( categorical==FALSE)
      {
        ## sliced.variance <- SVS( X, Y, p, m1, n, H1 )
        ## keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:floor(sqrt(n))] )
        ## keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
        keep.ind <- c(1:p)
        
        X <- X[, keep.ind]
        
        X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
        ORD <- order( Y )
        grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
        X.stand.ord <- X[ ORD, ] - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
    
        
        ## for( h in 1:H )
        ##   {
        ##     x.sliced <- X.stand.ord[ (1+(h-1)*m):(h*m) , ]
        ##     X.H[h, ] <- apply( x.sliced, 2, mean )
        ##   }
        
        ## Another way to calculate x.sliced.mean X_H. Note that X_H can be written as MX where M is a block diagonal matrix.
        M <- diag( H ) %x% matrix( 1, nrow=1,ncol= m)/m
        X.H <- M%*% X.stand.ord
      }else{
        Y.unique <- unique(Y)
        H <- length( Y.unique )
        ORD <- which( Y==Y.unique[1] )
        
        nH <- sum( Y == Y.unique[1] )
        for( i in 2:H )
          {
            ORD <- c( ORD, which(Y==Y.unique[i]) )
            nH <- c(nH, sum( Y==Y.unique[i] ) )
          }
        X <- X[ORD, ]

        M <- matrix( 0,  nrow=H, ncol=n )
        M[ 1, 1:nH[1] ] <- 1/nH[1]
        for(i in 2:H)
          M[ i, (sum(nH[1:(i-1)])+1):sum(nH[1:i]) ] <- 1/nH[i]
        x.sliced.mean <- M%*%X
        ## sliced.variance <- apply( x.sliced.mean, 2, var )
        ## keep.ind <- sort( order( sliced.variance, decreasing=TRUE)[1:n] )
        keep.ind <- c(1:p)
        X <- X[, keep.ind]
        
        X.H <- matrix(0, nrow=H, ncol= dim(X)[2] )
        grand.mean <- matrix( apply(X, 2, mean ), nrow=1, ncol=dim(X)[2] )
        X.stand.ord <- X - grand.mean %x% matrix(1, nrow=dim(X)[1], ncol=1)
        X.H <- M%*% X.stand.ord

      }
      
    eta <- array(0, c(p, no.dim) )
    LambdaHat <- t(X.H) %*% X.H/H
    temp <- eigen( LambdaHat )
    for( ii in 1:no.dim)
      eta[,ii] <- temp$vectors[,ii]
      

    if(no.dim>1){
      lars.fit.cv <- cv.glmnet( t(X.stand.ord)%*%X.stand.ord, eta, family="mgaussian", nfolds=nfolds )
    }else{
      lars.fit.cv <- cv.glmnet( t(X.stand.ord)%*%X.stand.ord, eta, nfolds=nfolds )
    }
    ind <- max( which( lars.fit.cv$cvm==min(lars.fit.cv$cvm) ) )
    if(ind==1)
      ind <- 2
    ## two ways of choosing the threshold. 1. choose the one with the smallest cvm; 2. choose the one with a significant smallest cvm. Namely, choose the one such that the lower bound is higher than the upper bound of the one with the smallest cvm.
    
    ## lambda <- lars.fit.cv$lambda[ max( ( lars.fit.cv$cvlo[1:ind] >= lars.fit.cv$cvup[ind] ) * c(1:ind) ) ]
    lambda <- lars.fit.cv$lambda[ind]
    if(no.dim>1){
      lars.fit <- glmnet(t(X.stand.ord)%*%X.stand.ord, eta, lambda=lambda, family="mgaussian")
      for(ii in 1:no.dim)
        beta.hat[keep.ind, ii] <- as.double( lars.fit$beta[[ii]] )
    }else{
      lars.fit <- glmnet(t(X.stand.ord)%*%X.stand.ord, eta, lambda=lambda)
      beta.hat[ keep.ind ] <- as.double( lars.fit$beta )
    }
    
    beta.hat
   
  }



