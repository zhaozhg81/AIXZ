library(fields)

###################
# Another example #
###################

# Download zip-file "chap4druguse" 
#   from http://biostatistics.iop.kcl.ac.uk/publications/everitt/
# Unzip file into your working directory

# These data were collected by Huba et al (1981).
# The data describe drug usage rates for 1634 students in 
#   7th-9th grade in 11 schools in the area of Los Angelos
# Responses were on a five point scale:
#  1 = never tried
#  2 = only once
#  3 = a few times
#  4 = many times
#  5 = regularly

# read correlation matrix:
druguse.cor<-source("./data/druguse.dat")$value
druguse.cor

# plot correlation matrix:
breaks <- seq(from=-0.5,to=0.62,length=65)
hmcols <- tim.colors(64)
image.plot(x=c(1:13), y=c(1:13), z=druguse.cor, zlim=c(-0.5, 0.62), col=tim.colors(64), breaks=breaks ,
      xlab="", ylab="", main="Image plot of correlation matrix", cex.lab=1.5)
row.names(druguse.cor)

# fit factor models with 2, 5, 6 factors:
res2 <- factanal(covmat = druguse.cor, factors=2, n.obs=1634,rotation="promax")
res2
res5 <- factanal(covmat = druguse.cor, factors=5, n.obs=1634)
res5
res6 <- factanal(covmat = druguse.cor, factors=6, n.obs=1634)
res6

# compare fitted correlation matrix to true correlation matrix:
fitted <- res2$loadings %*% t(res2$loadings) + diag(res2$uniquenesses)
round(druguse.cor-fitted, 2)
sum( (druguse.cor-fitted)^2 )
    

fitted6 <- res6$loadings %*% t(res6$loadings) + diag(res6$uniquenesses)
round(druguse.cor-fitted6, 2)
sum( (druguse.cor-fitted6)^2 )

# compare results:
res5.varimax <- factanal(covmat = druguse.cor, factors=5, n.obs=1634, rotation="varimax")
res5.varimax$loadings
GPForth( loadings(res5.varimax), method="quartimax") 
·

## Note that interpreting the unrotated factor loadings seems harder:
res5.none <- factanal(covmat = druguse.cor, factors=5, n.obs=1634, rotation="none")
res5.none$loadings

res5$loadings
