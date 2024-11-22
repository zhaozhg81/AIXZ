
mydata=read.table("data/3502/leukemia.txt",header=T)

out=glm(REMISS~.,data=mydata,family=binomial)

summary(out)

out0=glm(REMISS~1,data=mydata,family=binomial)

anova(out0,out,test="Chisq")

# check multicollineariy among predictors

round(cor(mydata[,-1]),3)

##################################################

library(MASS)

step=stepAIC(out, direction="backward")

step=stepAIC(out0, direction="forward",scope = list(lower = out0,
                                                   upper = out))