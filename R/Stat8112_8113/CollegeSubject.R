library(psych)
library(GPArotation)

## read the dataset into R variable using the read.csv(file) function
data <- read.csv("./data/CollegeSubject.csv")

## calculate the correlation matrix
corMat  <- cor(data)
##display the correlation matrix
corMat


res1 <- factanal(data, factors = 1, rotation = "none")
res2 <- factanal(data, factors = 2, rotation = "none")
res3 <- factanal(data, factors = 3, rotation = "none")
res1
res2



## Check the restriction, diagonal
round( t(res2$loadings) %*% solve( diag( res2$uniquenesses ) ) %*% res2$loadings, digits=2)

# compare different rotations:
res2.none <- factanal(data, factors=2, rotation="none")    # no rotation
res2.varimax <- factanal(data, factors=2, rotation="varimax") # default
res2.default <- factanal(data, factors=2)				# same as varimax
res2.promax <- factanal(data, factors=2, rotation="promax")  # oblique rotation

## Check the restriction, not diagonal, because the direction has been rotated.
round( t(res2.varimax$loadings) %*% solve( diag( res2.varimax$uniquenesses ) ) %*% res2.varimax$loadings, digits=2)

## What about other rotations such as "quartimax"?
## Need to use the package GPARotation "Gradient Projection Algorithm Rotation for Factor Analysis"
res2.quartimax <- GPForth( loadings( res2), method="quartimax")
res3.quartimax <- GPForth( loadings( res3), method="quartimax")


# note the uniquenesses (variances of specific factors u_i) are the same:
res2.none$uniquenesses
res2.varimax$uniquenesses
res2.default$uniquenesses
res2.promax$uniquenesses


#########################
# Compute factor scores #
#########################

res2.t <- factanal(data, factors=2, rotation="varimax", score="regression")   
plot(res2.t$scores)

## Consider a student with strong math/quantitaive skills

data[151,]
res2.t$scores[151,]
res2.t$loadings %*% res2.t$scores[151,]
