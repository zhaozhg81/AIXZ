## Delete dubious observation
# check = apply(dat[,1:10], 1, function(tl) length(table(tl)))
# fake = which(as.numeric(check)==1)
# dat = dat[-fake,]
# save(dat, file = "data.RData")
## Load data and packages
load("data/Movie_Survey.RData")
# data summary
library(psych)
des = describe(dat)

features = dat[1:10]
## Pearson Correlation
pear_cor = cor(features)
cor.plot(pear_cor, numbers=T, upper=FALSE, main = "Pearson Correlation", show.legend = FALSE)


## Polychoric correlation
poly_cor = polychoric(features)
rho = poly_cor$rho
### Thresholds/Scaling results
poly_cor$tau

cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)


## Simulation to compare pearson correlation and Polychoric correlation
library(mvtnorm)
SIGMA = matrix( 0.8, nrow=5,ncol=5)
diag(SIGMA) = 1
X = rmvnorm(100, mean=rep(0,5), sigma= SIGMA )
Y = (X < -1.5) + (X < -1) + (X < 1) + (X <1.5) + (X<10)
cor(Y)
polychoric(Y)$rho

  
# Scree plot
fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")


# Polychoric factor analysis
poly_model = fa(features, nfactor=3, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings
fa.diagram(poly_model, cut=0.5, e.cut=0.5)


factanal(covmat=rho, factors=2)