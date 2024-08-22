mydata=read.table("C:/Users/dongy/Desktop/linear regression/influence2.txt",header=T)

head(mydata)

x=mydata$x
y=mydata$y

lm(y~x)

lm(y[-21]~x[-21])

plot(x,y)
points(x[21],y[21],col="red")

abline(a=2.958,b=5.037)
abline(a=1.732,b=5.117,lty=2)

legend("bottomright", inset=.05, lty=c(1,2),
       c("with red point","without red point"))

#####################

n=length(y)

newx=cbind(1,x)

h=newx%*%solve(t(newx)%*%newx)%*%t(newx)

lev=diag(h)

yhat=h%*%y

res=y-yhat

mse=sum(res^2)/(n-2)

r_res=res/sqrt(mse*(1-lev))

t_res=r_res*sqrt((n-2-1)/(n-2-r_res^2))

out=lm(y~x)
cooks.distance(out)
