mydata=read.table("C:/Users/dongy/Desktop/linear regression/leukemia.txt",header=T)


##################################################
# model with only LI

newdata=data.frame(y=mydata$REMISS,x=mydata$LI)

logmodel = glm(y~., data = newdata, family = binomial)

cutoffs = seq(0.1,0.9,0.1)
#cutoffs = seq(0.01,0.99,0.01)
accuracy = NULL
for (i in seq(along = cutoffs)){
  prediction = ifelse(logmodel$fitted.values >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy = c(accuracy,length(which(newdata$y ==prediction))/length(prediction)*100)
}

par(mex=.79)
plot(cutoffs, accuracy, type='l', xlab="Cutoff Level", ylab = "Accuracy %",
     main="only LI")

##################################################
# model with all predictors

newdata=mydata
names(newdata)[1]="y"
logmodel = glm(y~., data = newdata, family = binomial)

cutoffs = seq(0.1,0.9,0.1)
#cutoffs = seq(0.01,0.99,0.01)
accuracy = NULL
for (i in seq(along = cutoffs)){
  prediction = ifelse(logmodel$fitted.values >= cutoffs[i], 1, 0) #Predicting for cut-off
  accuracy = c(accuracy,length(which(newdata$y ==prediction))/length(prediction)*100)
}

par(mex=.79)
plot(cutoffs, accuracy, type='l', xlab="Cutoff Level", ylab = "Accuracy %",
     main="all predictors")