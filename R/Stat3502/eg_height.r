library(scatterplot3d) # This library will allow us to draw 3d plot


mydata=read.table("data/3502/height.txt",header=T)
# change the directory to your own

str(mydata)

y=mydata$Height
x1=mydata$momheight
x2=mydata$dadheight

par(mfrow=c(1, 2))

plot(x1,y,xlab="mom height",ylab="", cex.lab=1.2)
title(ylab="daughter height", mgp=c(2,1,0), cex.lab=1.2)

plot(x2,y,xlab="dad height",ylab="", cex.lab=1.2)
title(ylab="daughter height", mgp=c(2,1,0), cex.lab=1.2)


par(mfrow=c(1, 1))

dataset = cbind.data.frame(x1,x2,y)

plot3d <- scatterplot3d(x1,x2,y,
                        angle=55, scale.y=0.7, pch=16, color ="red", 
                        xlab="mom height",ylab ="dad height",zlab="daughter height"
                        )
my.lm<- lm(y ~ x1 + x2,data=dataset)
plot3d$plane3d(my.lm, lty.box = "solid")

y_hat=my.lm$fitted.values

plot3d$points3d(x1,x2,y_hat,col="blue", type="h", pch=16)