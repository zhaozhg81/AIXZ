
x1=seq(-1,1,length=20)
x2=x1^2


par(mfrow=c(1, 2))

plot(x1[11:20],x2[11:20],xlab="x1",ylab="x2",cex.main=.7,main="correlation= 0.97")

plot(x1,x2,cex.main=.7,main="correlation = 0")

par(mfrow=c(1, 1))

cor(x1[11:20],x2[11:20])

cor(x1,x2)