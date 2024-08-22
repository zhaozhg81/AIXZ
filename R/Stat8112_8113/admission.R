library(aod)
library(ggplot2)

admit <- read.table("./data/admission.csv",sep=",",header=TRUE)
admit[,4] <- as.factor( admit[,4] ) ## Prepare the data, make the ranking as a categorical variable

## 
admit.logit <- glm( cbind(admit, 1-admit) ~ gre + gpa + rank, family=binomial("logit" ), data=admit )
summary( admit.logit )
confint( admit.logit, level=0.95 )
1-pchisq(  admit.logit$null.deviance - admit.logit$deviance, admit.logit$df.null - admit.logit$df.residual )

##
admit.m1 <- glm( cbind(admit, 1-admit) ~ gre + rank, family=binomial("logit" ), data=admit )
anova( admit.m1, admit.logit )

admit.m2 <- glm( cbind(admit, 1-admit) ~ gpa + rank, family=binomial("logit" ), data=admit )
anova( admit.m2, admit.logit )


## 
1-pchisq(  admit.logit$deviance, admit.logit$df.residual )


## 
library(aod)
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(admit.logit), Sigma = vcov(admit.logit), L = l)


## 
exp(cbind(OR = coef(admit.logit), confint(admit.logit)))


## 

newdata1 <- with(admit, data.frame( expand.grid ( c(min(gre):max(gre) ), factor(1:4)), gpa = mean(gpa) ) )
colnames( newdata1 ) <- c("gre","rank","gpa" )
newdata1$prob <- predict( admit.logit, newdata=newdata1, type="response" )
ggplot(newdata1, aes(gre, prob, color=rank, group=rank) ) + geom_line()


newdata2 <- with(admit, data.frame( expand.grid ( seq(min(gpa),max(gpa),0.01 ), factor(1:4)), gre = mean(gre) ) )
colnames( newdata2 ) <- c("gpa","rank","gre" )
newdata2$prob <- predict( admit.logit, newdata=newdata2, type="response" )
ggplot(newdata2, aes(gpa, prob, color=rank, group=rank) ) + geom_line()
