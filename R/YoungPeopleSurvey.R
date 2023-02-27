## Delete dubious observation
# check = apply(dat[,1:10], 1, function(tl) length(table(tl)))
# fake = which(as.numeric(check)==1)
# dat = dat[-fake,]
# save(dat, file = "data.RData")
## Load data and packages
load("data/Young_People_Survey.RData")
# data summary
library(psych)
des = describe(dat)
knitr::kable(des[,c("min", "max", "mean", "median", "skew", "kurtosis")], main = "Data Summary")


features = dat[1:10]
## Pearson Correlation
pear_cor = cor(features)
cor.plot(pear_cor, numbers=T, upper=FALSE, main = "Pearson Correlation", show.legend = FALSE)


## Polychoric correlation
poly_cor = polychoric(features)
rho = poly_cor$rho
save(rho, file = "polychoric")
### Thresholds/Scaling results
poly_cor$tau

cor.plot(poly_cor$rho, numbers=T, upper=FALSE, main = "Polychoric Correlation", show.legend = FALSE)


load("polychoric")
# Scree plot
fa.parallel(rho, fm="pa", fa="fa", main = "Scree Plot")


# Polychoric factor analysis
poly_model = fa(features, nfactor=3, cor="poly", fm="mle", rotate = "none")
poly_model$loadings
fa.diagram(poly_model)
