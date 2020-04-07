library(VGAM)
library(nnet)
library(exactLoglinTest)
library(reshape2)
data(alligator.dat)


lodds <- function(x,y){log((x+.5)/(y+.5))}

y <- t( matrix( alligator.dat[,1], nrow=5,ncol=16) )
ind <- c(0:15)*5+1

## gl: generate factors by specifying the pattern of their levels;
sizegroup <- gl(2,1,16)
gender <- gl(2,2,16)
lake <- gl(4,4,16)
alligator <- data.frame(count=y, size=sizegroup, gender=gender, lake=lake)
alligator$size <- as.factor( alligator$size)
alligator$gender <- as.factor( alligator$gender)
alligator$lake <- as.factor( alligator$lake)



## Fit the model
multi.model.para <- vglm( cbind(count.1,count.2,count.3,count.4,count.5)~ size+gender+lake, data=alligator, fam=multinomial(parallel=TRUE~size+gender+lake-1) )


multi.model <- vglm( cbind(count.1,count.2,count.3,count.4,count.5)~ size+gender+lake, data=alligator, fam=multinomial(parallel=FALSE) )
lrtest( multi.model.para, multi.model )


## maximum likelihood estimates
estimate <- matrix( coef( multi.model), ncol=6 )
colnames(estimate)=c("intercept", "size2", "gender2", "lake2", "lake3", "lake4")
rownames(estimate)=c("food1", "food2", "food3", "food4" )
estimate


## Fitted probabilities.
prob <- predict( multi.model, type="response" )
colnames( prob ) <- c("1", "2", "3", "4", "5")
pred.prob <- data.frame( prob=prob, size=sizegroup, gender=gender, lake=lake)

library(tidyr)
pred.prob <- pred.prob%>% spread(variable,value)

