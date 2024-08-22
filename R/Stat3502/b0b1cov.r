par(mfrow = c(1, 1))  

par(mar=c(2,2,2,2))

plot(1,1, ylab="",xlab="", main=expression(bar(x)>0), xlim=c(-1,2),ylim=c(-1,2))

text(1,1.2,expression(paste("(",bar(x),",",bar(y),")")))

abline(v=0)
abline(h=0)

abline(a=0,b=1,col="red")

abline(a=1/2,b=1/2,col="blue")

#########################

plot(-1,1, ylab="",xlab="", main=expression(bar(x)<0), xlim=c(-2,1),ylim=c(-1,2))

text(-1,1.2,expression(paste("(",bar(x),",",bar(y),")")))

abline(v=0)
abline(h=0)

abline(a=0,b=-1,col="red")

abline(a=1/2,b=-1/2,col="blue")