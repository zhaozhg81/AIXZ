


mydata = read.csv("data/3502/cwp.csv",header=T)

names(mydata)=c("years","cases","miners","prop")

mydata

################################################
# change data to a format that glm recognizes
# y should be 0 and 1

x=rep(mydata$years,mydata$miners)

tmp=cbind(mydata$cases,mydata$miners-mydata$cases)

y=numeric(0)
for(i in 1:8){ 
  y=c(y,rep(c(1,0),tmp[i,]))
}

newdata=data.frame(x=x,y=as.factor(y))

head(newdata)

################################################
# use glm to run logistic regression

out=glm(y~x,family=binomial,data=newdata)

summary(out)

out0=glm(y~1,family=binomial)

anova(out0,out,test="Chisq")


################################################
# plot fitted (observed) proportions v.s. years

yhat=out$fitted.values

plot(x,yhat,ylab="proportion",xlab="years")
points(mydata$years,mydata$prop,pch=4,col="red")
legend("topleft",legend=
         c("estimated proportions","observed proportions"),pch=c(1,4),col=c("black","red"))


###########################################
# plot estimated pi(x) (logistic function)

a=seq(min(x),max(x),by=.1)

beta=out$coefficients

b=exp(beta[1]+beta[2]*a)/(1+exp(beta[1]+beta[2]*a))

plot(a,b,xlab="years",ylab="Severe case",lty=1,type="l",ylim=c(0,1))

points(x,as.numeric(y))


########################################################
# confidence interval and prediction in logistic regression

confint(out)

# predict the probability
predict(out,data.frame(x=30),type="response")


# predict the log odds
predict(out,data.frame(x=30))



