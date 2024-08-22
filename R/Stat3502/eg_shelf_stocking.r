mydata=read.csv("C:/Users/dongy/Desktop/linear regression/shelf_stocking.csv",header=T)

str(mydata)

colnames(mydata)=c("time","cases_stocked")

str(mydata)

y=mydata$time
x=mydata$cases_stocked

plot(x,y,xlab="cases",ylab="time",cex.lab=1.2)

bhat=sum(x*y)/sum(x^2)

abline(a=0,b=bhat,col="red")