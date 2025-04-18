mydata=read.table("data/3502/influence3.txt",header=T)

head(mydata)

x=mydata$x
y=mydata$y

## leverage value
newx=cbind(1,x)
h=newx%*%solve(t(newx)%*%newx)%*%t(newx)
lev=diag(h)



out=lm(y~x)
cooks.distance(out)

lm(y~x)

lm(y[-21]~x[-21])

plot(x,y)
points(x[21],y[21],col="red")

abline(a=2.468,b=4.927)
abline(a=1.732,b=5.117,lty=2)

legend("bottomright", inset=.05, lty=c(1,2),
       c("with red point","without red point"))