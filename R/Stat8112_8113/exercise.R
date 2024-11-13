
library(car)

mydata=read.table("C:/Users/dongy/Desktop/linear regression/exercise.txt",header=T)

head(mydata)

y=mydata$igg

x=mydata$oxygen

plot(x,y)

x2=x^2

out0=lm(y~x+x2)

out0$coefficients

vif(out0)

newx=seq(min(x),max(x),length=100)
newy=-1464.4042284+88.3070970*newx-0.5362473*newx^2
points(newx,newy,type="l",col="red")

cor(x,x2)

##########################

cx=x-mean(x)
cx2=cx^2
out=lm(y~cx+cx2)
vif(out)

cor(cx,cx2)

par(mfrow=c(1, 2))

plot(x,x2,cex.main=.7,main="correlation= 0.995")

plot(cx,cx2,cex.main=.7,main="correlation= 0.220")

par(mfrow=c(1, 1))

##############################

par(mfrow=c(1, 2))

plot(x,y)

newx=seq(min(x),max(x),length=100)
newy=-1464.4042284+88.3070970*newx-0.5362473*newx^2
points(newx,newy,type="l",col="red")

out$coefficients

plot(cx,y,xlab="centered x")

newx=seq(min(cx),max(cx),length=100)
newy=1632.1961868+33.9995483 *newx-0.5362473*newx^2
points(newx,newy,type="l",col="red")

par(mfrow=c(1, 1))


########################################
# confidence interval for mean response 

df0=data.frame(y=y,x1=x,x2=x2)
out0=lm(y~.,data=df0)
new_obs = data.frame(x1=60,x2=60^2)

predict(out0,new_obs, interval = "confidence")


df1=data.frame(y=y,x1=cx,x2=cx2)
out=lm(y~.,data=df1)
new_obs = data.frame(x1=60-mean(x),x2=(60-mean(x))^2)

predict(out,new_obs, interval = "confidence")
