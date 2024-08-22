x=c(1,2,3,4)
y=c(2,5,6,9)

#x=c(1,2,3,10)
#y=c(2.1,3.8,5.2,2.1)

newx=cbind(1,x)

h=newx%*%solve(t(newx)%*%newx)%*%t(newx)

lev=diag(h)

yhat=h%*%y

res=y-yhat

mse=sum(res^2)/(4-2)

r_res=res/sqrt(mse*(1-lev))

t_res=r_res*sqrt((4-2-1)/(4-2-r_res^2))

lm(y~x)

lm(y[-4]~x[-4])

plot(x,y)
points(10,2.1,col="red")
abline(a=3.82,b=-.13)
abline(a=.6,b=1.55,lty=2)


