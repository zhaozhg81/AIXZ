library(VGAM)
library(mvtnorm)
library(dr)
library(clime)
library(CompQuadForm)
library(psych)
library(energy)
library(glmnet)
library(ggplot2)
library(nsprcomp)


wine <- read.table("data/wine.data",sep=",")

p <- dim(wine)[2]-1
n <- dim(wine)[1]

X <- wine[,2:14]
Y <- wine[,1]
X <- as.matrix(X)
Y <- as.matrix(Y)

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


## PCA
wine.pca <- princomp(X)
comp.pca <- data.frame( wine.pca$scores[,1], wine.pca$scores[,2],  Y )

colnames( comp.pca ) <- c("C1","C2","Y")
p1 <- ggplot( comp.pca, aes( x=C1, y=C2 ) )
p1 + geom_point( aes(color=factor( Y) )) + theme( legend.position="none" ) + ggtitle("PCA")

## Sparse PCA
wine.spca <- nsprcomp(X)
wine.spca$score <- X%*% wine.spca$rotation
comp.spca <- data.frame( wine.spca$score[,1], wine.spca$score[,2],  Y )

colnames( comp.spca ) <- c("C1","C2","Y")
p1 <- ggplot( comp.spca, aes( x=C1, y=C2 ) )
p1 + geom_point( aes(color=factor( Y) )) + theme( legend.position="none" ) + ggtitle("Sparse PCA")



## ## SIR LASSO
sir.lasso.beta <-  LassoSIR( X=X, Y=Y, choosing.d="manual", solution.path=FALSE, categorical=TRUE, nfolds=n, screening=FALSE, no.dim=2)$beta

sir.lasso.beta[,1] <- sir.lasso.beta[,1]/sqrt( sum( sir.lasso.beta[,1]^2 ) )
sir.lasso.beta[,2] <- sir.lasso.beta[,2]/sqrt( sum( sir.lasso.beta[,2]^2 ) )

comp.sir.lasso <- X %*% sir.lasso.beta 

comp.sir.lasso.data <- data.frame( comp.sir.lasso[,1], comp.sir.lasso[,2], Y)

colnames( comp.sir.lasso.data ) <- c("C1","C2","Y")
p1 <- ggplot( comp.sir.lasso.data, aes( x=C1, y=C2 ) )
p1 + geom_point( aes(color=factor( Y) )) + theme( legend.position="none" ) + ggtitle("LASSO-SIR")




## LASSO
## ## LASSO
lars.fit.cv <- cv.glmnet( X, Y, family="multinomial" ) 
## two ways of choosing the threshold. 1. choose the one with the smallest cvm; 2. choose the one with a significant smallest cvm. Namely, choose the one such that the lower bound is higher than the upper bound of the one with the smallest cvm.
ind <- which( lars.fit.cv$cvm==min(lars.fit.cv$cvm) )

lambda <- lars.fit.cv$lambda[ max( ( lars.fit.cv$cvlo[1:ind] >= lars.fit.cv$cvup[ind] ) * c(1:ind) ) ]
lambda <- lars.fit.cv$lambda[ind]
lars.fit <- glmnet(X, Y, lambda=lambda, family="multinomial")
comp.lasso <- array(0, c(178, 3) )

comp.lasso.data <- data.frame( (X %*% lars.fit$beta$'3')[,1], (X %*% lars.fit$beta$'2')[,1], (X %*% lars.fit$beta$'1')[,1], Y )
colnames( comp.lasso.data) <- c("C1", "C2", "C3", "Y")



p1 <- ggplot( comp.lasso.data, aes( x=C1, y=C2 ) )
p1 + geom_point( aes(color=factor( Y) )) + theme( legend.position="none" ) + ggtitle("LASSO")
