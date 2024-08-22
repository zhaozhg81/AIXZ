x1=2
y1=1
x2=3
y2=0
x0=0
y0=0

par(mfrow=c(2,2))

##############################

plot(1, axes=FALSE,type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-.5,3.5),ylim=c(-1,1.5))
arrows(x0, y0, x1, y1)
text(2,1.3,"y vector")
arrows(x0, y0, x2, y2)
text(3,-.3,"x vector")

##############################

plot(1, axes=FALSE,type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-.5,3.5),ylim=c(-1,1.5))
arrows(x0, y0, x1, y1)
text(2,1.3,"y vector")
arrows(x0, y0, x2, y2)
text(3,-.3,"x vector")

abline(v=2,col="red",lty=2)

##############################

plot(1, axes=FALSE,type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-.5,3.5),ylim=c(-1,1.5))
arrows(x0, y0, x1, y1)
text(2,1.3,"y vector")
arrows(x0, y0, x2, y2)
text(3,-.3,"x vector")


abline(v=2,col="red",lty=2)


arrows(0,0,2,0,col="purple")
text(1,-.3,"y hat vector",col="purple")

##############################

plot(1, axes=FALSE,type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-.5,3.5),ylim=c(-1,1.5))
arrows(x0, y0, x1, y1)
text(2,1.3,"y vector")
arrows(x0, y0, x2, y2)
text(3,-.3,"x vector")


arrows(2,-0,2,1,col="red")
text(2.2,.5,"residual vector",col="red")

arrows(0,0,2,0,col="purple")
text(1,-.3,"y hat vector",col="purple")