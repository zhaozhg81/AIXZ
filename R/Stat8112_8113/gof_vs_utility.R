data1 <- read.table("./data/data1.txt",header=TRUE, sep=",")
data1.logit <- glm( cbind(Y, 10-Y)~X, family=binomial("logit"), data=data1 )
summary( data1.logit )
## Model utility test
pvalue.util.1 <- 1- pchisq( data1.logit$null.deviance - data1.logit$deviance, data1.logit$df.null - data1.logit$df.residual )
## goodness-of-fit test
pvalue.gof.1 <- 1 - pchisq( data1.logit$deviance, data1.logit$df.residual )


data2 <- read.table("./data/data2.txt",header=TRUE, sep=",")
data2.logit <- glm( cbind(Y, 10-Y)~X, family=binomial("logit"), data=data2 )
summary( data2.logit )
## Model utility test
pvalue.util.2 <- 1- pchisq( data2.logit$null.deviance - data2.logit$deviance, data2.logit$df.null - data2.logit$df.residual )
## goodness-of-fit test
pvalue.gof.2 <- 1 - pchisq( data2.logit$deviance, data2.logit$df.residual )
