library(BET)
library(MASS)

#=========================================================================#
data.generator <- function(N,scenario,para){
  if (scenario=="gau"){
    ## x = mvrnorm(N,c(0,0),matrix(c(1,0.35,0.35,1),2))  + mvrnorm(N,c(0,0),matrix(c(1,0,0,1)*para*sqrt(2),2))
    ## x = mvrnorm(N,c(0,0),matrix(c(1,para,para,1),2) )
    x = mvrnorm(N,c(0,0),matrix(c(1,0.05+0.3*para,0.05+0.3*para,1),2) )
    ## x = mvrnorm(N,c(0,0),matrix(c(1,0.05+0.3*para,0.05+0.3*para,1),2))  + mvrnorm(N,c(0,0),matrix(c(1,0,0,1)*para*sqrt(2),2))

  }
  
  if (scenario=="cir"){
    
    theta = runif(N,-pi,pi);
    x1 = cos(theta)+rnorm(N,0,(0.23 * para+0.05)*(4)^(2/3));
    x2 = sin(theta)+rnorm(N,0,(0.23 * para+0.05)*(4)^(2/3));
    x = cbind(x1,x2)
  }
  if (scenario=="sin"){
    x1 = runif(N,-1,1);
    x2 = sin(4*pi*x1)+rnorm(N,0,8*(0.3*para+0.05) );
    x = cbind(x1,x2)
  }
  if (scenario=="cb"){
    epsilon <- rnorm(N,sd=(0.05+0.3*para)*sqrt(1));
    epsilon1 <- rnorm(N,sd=(0.05+0.3*para)*sqrt(1))
    W <- sample(c(1,2,3),N,replace=T);
    V1 <- sample(c(1,3,5),N,replace=T)
    V2 <- sample(c(2,4),N,replace=T);
    W.odd <- W!=2
    x1 <- W+epsilon;
    x2 <- W.odd*(V1+4*epsilon1)+(1-W.odd)*(V2+4*epsilon1);
    x <- cbind(x1,x2)
  }
  if (scenario=="concen"){
   
    epsilon1 <- rnorm(N,sd=sqrt( (0.3*para+0.05)/2 ) )
    epsilon2 <- rnorm(N,sd=sqrt( (0.3*para+0.05)/2 ) )
    U <- runif(N)*2*pi
    R <- sample(c(1,1/2),N,replace=T)
    x1 <- R*cos(U)+epsilon1
    x2 <- R*sin(U)+epsilon2
    x = cbind(x1,x2)
  }
  
  if (scenario=="parabolic"){
    epsilon <- rnorm(N,sd=(0.35*para+0.15)/2)
    U <- runif(N)
    x1 <- U
    x2 <- (x1-0.5)^2+1.5*epsilon
    x = cbind(x1,x2)
  }
  return(x)
}


N <- 128
p <- 5

scen = c("gau","cir","sin","cb","concen","parabolic")


id <- 2
para <- 0.3
  
X <- array( rnorm(N*p), c(N, p))
rep.ind <- sample(c(1:p),2)

X[ , rep.ind ] <- data.generator(N,scen[id],para)

p.value.BET <- array(0, c(p,p) )
p.value.pearson <- array(0, c(p,p) )

for(i in 2:p)
  for(j in 1:(i-1))
  {
    p.value.pearson[i,j] <- cor.test( X[,i], X[,j] )$p.value
    p.value.pearson[j,i] <- p.value.pearson[i,j]

    p.value.BET[i,j] <- BEAST( cbind(X[,i],X[,j]), d=2)$p.value
    p.value.BET[j,i] <- p.value.BET[i,j]

    print( paste("i=",i,"; j=",j,sep="") )
  }

diag( p.value.BET) <- 1
diag( p.value.pearson ) <- 1
