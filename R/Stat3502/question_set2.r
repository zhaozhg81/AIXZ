mydata=read.table("C:/Users/dongy/Desktop/linear regression/influence2.txt",header=T)

head(mydata)

x=mydata$x
y=mydata$y

lm(y~x)

lm(y[-21]~x[-21])

#plot(x,y)

par(mfrow=c(1, 2))

plot(x[-21],y[-21],xlab="x",ylab="y")
points(x[21],y[21],col="black",pch=4)

##########################################

mydata=read.table("C:/Users/dongy/Desktop/linear regression/influence3.txt",header=T)

head(mydata)

x=mydata$x
y=mydata$y

#plot(x,y)



plot(x[-21],y[-21],xlab="x",ylab="y",xlim=c(0,15),ylim=c(0,70))
points(x[21],y[21],col="black",pch=4)