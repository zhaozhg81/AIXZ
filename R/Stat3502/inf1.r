mydata=read.table("data/3502/influence1.txt",header=T)

head(mydata)

x=mydata$x
y=mydata$y

plot(x,y)

n=length(y)
ab_all=matrix(0,n,2)

for(i in 1:n){
  tmpy=y[-i]
  tmpx=x[-i]
ab_all[i,]=(lm(tmpy~tmpx))$coefficients
}

plot(ab_all[,1],ab_all[,2],xlim=c(0,6),ylim=c(4,6),
     xlab="estimated intercept",ylab="estimated slope")

########################
mydata=read.table("data/3502/influence4.txt",header=T)

head(mydata)

x=mydata$x
y=mydata$y

n=length(y)
ab_all=matrix(0,n,2)

for(i in 1:n){
  tmpy=y[-i]
  tmpx=x[-i]
  ab_all[i,]=(lm(tmpy~tmpx))$coefficients
}

plot(ab_all[,1],ab_all[,2],xlim=c(0,11),ylim=c(0,6),
     xlab="estimated intercept",ylab="estimated slope")
points(ab_all[21,1],ab_all[21,2],col="red")
text(ab_all[21,1]+.5,ab_all[21,2]-.5,"influential point deleted")
