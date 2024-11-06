
library(MASS)

mydata=read.table("data/3502/cement.txt",header=T)

head(mydata)


##########################################

## Forward regression
step( lm(y~1, data=mydata), scope=list(lower=~1, upper=~x1+x2+x3+x4), direction="forward")

## Backward regression
step( lm(y~x1+x2+x3+x4, data=mydata), direction="backward")

## step regression
step( lm(y~1, data=mydata), scope=list(lower=~1, upper=~x1+x2+x3+x4), direction="both")