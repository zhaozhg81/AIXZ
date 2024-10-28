mydata=read.table("data/3502/influence4.txt",header=T)

head(mydata)

x=mydata$x
y=mydata$y

out=lm(y~x)
cooks.distance(out)

lm(y~x)

lm(y[-21]~x[-21])

plot(x,y)
points(x[21],y[21],col="red")

abline(a=8.505,b=3.320)
abline(a=1.732,b=5.117,lty=2)

legend("bottomright", inset=.05, lty=c(1,2),
       c("with red point","without red point"))