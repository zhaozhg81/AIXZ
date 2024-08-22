mydata=read.table("C:/Users/dongy/Desktop/linear regression/height_foot.txt",header=T)

head(mydata)

y=mydata$foot
x=mydata$height

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

which.max(cooks.distance(out))


#28

max(cooks.distance(out))

lev[28]

r_res[28]

t_res[28]

lm(y[-28]~x[-28])
lm(y~x)

plot(mydata)
points(x[28],y[28],col="red")
abline(a=10.936,b=0.2334)
abline(a=0.2531,b=0.384,lty=2)
legend("topleft", inset=.05, lty=c(1,2),
                                       c("with red point","without red point"))

######################################

ab_all=matrix(0,n,2)

for(i in 1:n){
  tmpy=y[-i]
  tmpx=x[-i]
  ab_all[i,]=(lm(tmpy~tmpx))$coefficients
}

plot(ab_all[,1],ab_all[,2],
     #xlim=c(0,6),ylim=c(4,6),
     xlab="estimated intercept",ylab="estimated slope")

points(ab_all[28,1],ab_all[28,2],col="red")
text(ab_all[28,1]+3,ab_all[28,2]-.02,"influential point deleted")

