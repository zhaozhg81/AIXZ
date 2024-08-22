mydata=read.table("C:/Users/dongy/Desktop/linear regression/under.txt",header=T)

head(mydata)

x1=mydata$x1
x2=mydata$x2
y=mydata$y

plot(x1,y)
out0=lm(y~x1)
abline(a=coefficients(out0)[1],b=coefficients(out0)[2])

title(expression(R^2==0.88))

summary(out0)$r.squared

#################################


plot(x1,y)
points(x1[x2==0],y[x2==0],col="blue")
points(x1[x2==20],y[x2==20],col="red")

legend("topleft",        
       legend = c("x2=20", "x2=10","x2=0"),
       col = c("red","black","blue"),
       pch=c(1,1)  )  

#################################

out=lm(y~x1+x2)

summary(out)$r.squared

coefficients(out)

plot(x1,y)
points(x1[x2==0],y[x2==0],col="blue")
points(x1[x2==20],y[x2==20],col="red")

title(expression(R^2==0.997))

abline(a=coefficients(out)[1],b=coefficients(out)[2],col="blue")

abline(a=coefficients(out)[1]+10*coefficients(out)[3],b=coefficients(out)[2])

abline(a=coefficients(out)[1]+20*coefficients(out)[3],b=coefficients(out)[2],
col="red")

legend("topleft",        
       legend = c("x2=20", "x2=10","x2=0"),
       col = c("red","black","blue"),
       lty=c(1,1,1),pch=c(1,1)  )        


