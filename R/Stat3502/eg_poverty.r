mydata=read.table("./data/3502/poverty.txt",header=T)
# change the directory to your own

str(mydata)
y=mydata$Brth15to17
x=mydata$PovPct

#plot(x,y)
# a plain plot without x and y labels

plot(x,y,xlab="poverty rate",ylab="teen birth rate", cex.lab=1.5)

output=lm(y~x)

ab=output$coefficients

abline(a=ab[1],b=ab[2],col="red")