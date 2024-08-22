mydata=read.table("C:/Users/dongy/Desktop/linear regression/birthsmokers.txt",header=T)

head(mydata)

# print the first few observations to check whether data is imported properly

str(mydata)

# check the structure of the data

y=mydata$Wgt

x1=mydata$Gest

x2=as.numeric(mydata$Smoke)-1

head(x2)

# check that smoking is coded as x2=1 and non-smoking is coded as x2=0

out=lm(y~x1+x2)

summary(out)


out0=lm(y~1)

# fit an intercept-only model

anova(out0,out)

# how is the F-test statistic calculated?
# Refer to page 30 of lecture slides MLR.pdf

#(summary(out))$sigma
#(anova(out0,out))$F

n=length(y)

ind=(1:n)[x2==1]



tmp=(summary(out))$coefficients
beta_hat=tmp[,1]

plot(x1,y,xlab="gestation",ylab="weight")
points(x1[ind],y[ind],col="red")
points(x1[-ind],y[-ind],col="blue")


legend("topleft", inset=.05, pch=c(1,1),
       c("non-smokers","smokers"),  col=c("blue","red"))

# scatterplot

plot(x1,y,xlab="gestation",ylab="weight")
points(x1[ind],y[ind],col="red")
points(x1[-ind],y[-ind],col="blue")
abline(a=beta_hat[1],b=beta_hat[2],col="blue")
abline(a=beta_hat[1]+beta_hat[3],b=beta_hat[2],col="red")

legend("topleft", inset=.05, pch=c(1,1),lty=c(1,1),
       c("non-smokers","smokers"),  col=c("blue","red"))


# plot with fitted regression lines
