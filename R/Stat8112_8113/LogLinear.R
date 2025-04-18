source("./R/func/nncopy.R")

########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

## Exit poll
poll1.ori = read.table( "data/ExitPoll1.txt", header=TRUE )
poll.test <- chisq.test( poll1.ori[1:2,1:2], correct=FALSE )

poll1.total = sum( poll1.ori[1:2,1:2] )
## Likelihood ratio test

  
tmp = nncopy( unlist(poll1.ori[3,1:2]), 2, 1  )
tmp2 = nncopy( unlist(poll1.ori[1:2,3]),2, 2 )
mu.hat1 <- tmp * tmp2/poll1.total
  
lrt.test1 = 2 * sum( poll1.ori[1:2,1:2] * log( poll1.ori[1:2,1:2]/mu.hat1) )
  

poll1 <- data.frame( expand.grid( c("Men","Women"),c("Gore","Bush") ) ) 
colnames(poll1)=c("Gender","Candidate")
poll1$count <- c( unlist(   poll1.ori[1:2,1:2] ) )

poll1.loglinear <- glm( count ~ Gender + Candidate, family=poisson, data=poll1 )


##
poll2.ori <- read.table( "data/ExitPoll2.txt", header=TRUE )
poll2.total = sum( poll2.ori[1:5,1:2] )
poll2.test <- chisq.test( poll2.ori[1:5,1:2], correct=FALSE )

tmp = nncopy( unlist(poll2.ori[6,1:2]), 5, 1  )
tmp2 = nncopy( unlist(poll2.ori[1:5,3]),2, 2 )
mu.hat2 <- tmp * tmp2/poll2.total

lrt.test2 = 2 * sum( poll2.ori[1:5,1:2] * log( poll2.ori[1:5,1:2]/mu.hat2) )


poll2 <- data.frame( expand.grid( c("NoHS","HS","College","University","PG"),c("Gore","Bush") ) ) 
colnames(poll2)=c("Education","Candidate")
poll2$count <- c( unlist(   poll2.ori[1:5,1:2] ) )

poll2.loglinear <- glm( count ~ Education + Candidate, family=poisson, data=poll2 )

## ##########################################
## Education Aspiration

EduAspiration = read.csv("./data/EducationAspirations.csv")

tapply(EduAspiration$Count, list( EduAspiration$Social, EduAspiration$Education), sum)
tapply(EduAspiration$Count, list( EduAspiration$Social, EduAspiration$Education, EduAspiration$Plan), sum)

## Possible log-linear models to be considered
Edu.m1  = glm( Count~  Social + Education + Plan, family=poisson, data=EduAspiration) 
Edu.m2  = glm( Count~  Social * Education + Plan, family=poisson, data=EduAspiration) 
Edu.m3  = glm( Count~  Social * Plan + Education, family=poisson, data=EduAspiration) 
Edu.m4  = glm( Count~  Social + Education * Plan, family=poisson, data=EduAspiration) 
Edu.m5  = glm( Count~  Social * Education + Social * Plan, family=poisson, data=EduAspiration) 
Edu.m6  = glm( Count~  Social * Education + Education * Plan, family=poisson, data=EduAspiration) 
Edu.m7  = glm( Count~  Social * Plan + Education * Plan, family=poisson, data=EduAspiration) 
Edu.m8  = glm( Count~  Social * Education + Social * Plan + Education * Plan, family=poisson, data=EduAspiration) 
Edu.m9  = glm( Count~  Social * Education * Plan, family=poisson, data=EduAspiration) 

summary.tabulate = data.frame ( c("S+E+P","SE+P","SP+E","S+EP","SE+SP","SE+EP","SP+EP","SE+SP+EP") )
colnames(summary.tabulate)=c("Model")
summary.tabulate$Deviance = c( Edu.m1$deviance, Edu.m2$deviance , Edu.m3$deviance , Edu.m4$deviance , 
                               Edu.m5$deviance , Edu.m6$deviance , Edu.m7$deviance , Edu.m8$deviance  )
summary.tabulate$df = c( Edu.m1$df.residual,  Edu.m2$df.residual,  Edu.m3$df.residual,  Edu.m4$df.residual, 
                         Edu.m5$df.residual,  Edu.m6$df.residual,  Edu.m7$df.residual,  Edu.m8$df.residual)


## Logistic regressino model
EduAspiration2 <- EduAspiration[c(1:8)*2,]
colnames( EduAspiration2) <- c("Social","Education","Plan","Yes")
EduAspiration2$No <- EduAspiration[c(0:7)*2+1, 4]

Edu.log.m0 = glm( cbind(Yes, No)~1, family=binomial(link="logit"), data=EduAspiration2)
Edu.log.m1 = glm( cbind(Yes, No)~Social, family=binomial(link="logit"), data=EduAspiration2)
Edu.log.m2 = glm( cbind(Yes, No)~ Education, family=binomial(link="logit"), data=EduAspiration2)
Edu.log.m3 = glm( cbind(Yes, No)~ Social + Education, family=binomial(link="logit"), data=EduAspiration2)
Edu.log.m4 = glm( cbind(Yes, No)~ Social * Education, family=binomial(link="logit"), data=EduAspiration2)

summary.tabulate.logit = data.frame ( c("Null","S","E","S+E") )
colnames(summary.tabulate.logit)=c("Model")
summary.tabulate.logit$Deviance = c( Edu.log.m0$deviance, Edu.log.m1$deviance, Edu.log.m2$deviance, Edu.log.m3$deviance)
summary.tabulate.logit$df = c( Edu.log.m0$df.residual,   Edu.log.m1$df.residual,   Edu.log.m2$df.residual,   Edu.log.m3$df.residual )
