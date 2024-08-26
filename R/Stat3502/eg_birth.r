mydata=read.table("data/3502/birthsmokers.txt",header=T)
# change the directory to your own
str(mydata)

y=mydata$Wgt
x1=mydata$Gest
x2=mydata$Smoke

par(mfrow=c(1, 1))

#plot(x1,y,xlab="gestation",ylab="weight", cex.lab=1.5)

plot(x1[x2=="yes"],y[x2=="yes"],ylim=c(min(y),max(y)),
     xlab="gestation",ylab="weight", cex.lab=1.5,col="red")

points(x1[x2=="no"],y[x2=="no"],col="blue",pch=4)

# legend(x = "topleft",          # Position
#        pch = c(4, 1),           # point types
#        legend = c("non-smoker", "smoker"),  # Legend texts
#        col = c("blue", "red") )        # point colors

output=lm(y~x1+x2)

ab=output$coefficients

abline(a=ab[1],b=ab[2],col="blue")

abline(a=ab[1]+ab[3],b=ab[2],col="red",lty=2)

legend(x = "topleft",          # Position
       lty = c(1, 2),           # Line types
       legend = c("non-smoker fit", "smoker fit"),  # Legend texts
       col = c("blue", "red") )        # Line colors

