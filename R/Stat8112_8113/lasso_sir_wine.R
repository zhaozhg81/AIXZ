library(VGAM)
library(mvtnorm)
library(dr)
library(clime)
library(CompQuadForm)
library(psych)
library(energy)
library(glmnet)
library(ggplot2)

## source("../../DT_SIR.R")
source("./func/SIR_LASSO.R")




wine <- read.csv("../data/winequality-white.csv",sep=";")

p <- dim(wine)[2]-1
n <- dim(wine)[1]

X <- wine[,1:11]
Y <- wine[,12]
X <- as.matrix(X)
Y <- as.matrix(Y)

y <- matrix( 0, nrow=n, ncol=length( unique( Y ) ) )
for(i in 1:n )
  y[ i, Y[i]-2 ] <- 1
colnames( y ) <- c("s3","s4","s5","s6","s7","s8","s9")

X.mean <- array( apply( X, 2, mean ), c(1,p) )%x% array(1, c(n, 1) )
X.std <- array( sqrt( apply( X,2,var)), c(1,p))%x%array(1, c(n, 1) )

X <- (X-X.mean)/X.std

no.dim <- 2

H <- 16
m <- n/H
H1 <- H
m1 <- m
H2 <- H
m2 <- m



## postscript("wine_pca.eps",horizontal=FALSE)
## PCA
wine.pca <- princomp(X)
comp.pca <- data.frame( wine.pca$scores[,1], wine.pca$scores[,2],  Y, y )

colnames( comp.pca ) <- c("C1","C2","Y","s3","s4","s5","s6","s7","s8","s9")
pca.fit <- vglm( cbind(s3,s4,s5,s6,s7,s8,s9)~ C1 + C2, data=comp.pca, fam=multinomial(parallel=TRUE~C1+C2-1) )


p1 <- ggplot( comp.pca, aes( x=C1, y=C2 ) )
p1 + geom_point( aes(color=factor( Y) )) + theme( legend.position="none" ) + ggtitle("PCA")
## dev.off()

## postscript("wine_spca.eps",horizontal=FALSE)
## Sparse PCA
wine.spca <- nsprcomp(X)
wine.spca$score <- X%*% wine.spca$rotation
comp.spca <- data.frame( wine.spca$score[,1], wine.spca$score[,2],  Y )

colnames( comp.spca ) <- c("C1","C2","Y")
p1 <- ggplot( comp.spca, aes( x=C1, y=C2 ) )
p1 + geom_point( aes(color=factor( Y) )) + theme( legend.position="none" ) + ggtitle("Sparse PCA")
## dev.off()




## ## DT-SIR
## postscript("wine_dt_sir.eps",horizontal=FALSE)
tmp <- cov(X)
SigmaX.inv <- solve( tmp*( abs(tmp)> (  log(p)/sqrt(n))) )
dt.sir.beta <- SSIR(X, SigmaX.inv, Y, p, m1, m2, n, H1, H2, no.dim, categorical=TRUE )$beta
dt.sir.beta[,1] <- dt.sir.beta[,1]/sqrt( sum( dt.sir.beta[,1]^2 ) )
dt.sir.beta[,2] <- dt.sir.beta[,2]/sqrt( sum( dt.sir.beta[,2]^2 ) )

comp.dt.sir <- X %*% dt.sir.beta 

comp.dt.sir.data <- data.frame( comp.dt.sir[,1], comp.dt.sir[,2], Y)

colnames( comp.dt.sir.data ) <- c("C1","C2","Y")
p1 <- ggplot( comp.dt.sir.data, aes( x=C1, y=C2 ) )
p1 + geom_point( aes(color=factor( Y) )) + theme( legend.position="none" ) + ggtitle("DT SIR")

## dev.off()

