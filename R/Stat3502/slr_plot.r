
par(mfrow=c(1,2))

par(mar=c(1,1,1,1))

##############################

plot(1, axes=FALSE,type="n", xaxt="n", yaxt="n", xlab="", ylab="", xlim=c(-.5,4.5),ylim=c(-1,4))

arrows(.6,.2,1.4,.6)

text(1.6,.7,"one vector")

arrows(.6,.2,2.6,.2)

text(1.6,.1,"x vector")

arrows(.6,.2,2.5,3,col="blue")

text(2.5,3.2,"y vector",col="blue")

#abline(v=2.5,lty)

segments(0,0,1.6,.8)

segments(0,0,3,0)

segments(1.6,.8,4.6,.8)

segments(3,0,4.6,.8)

segments(2.5,4,2.5,.4,col="red",lty=2)

segments(2.5,-1,2.5,0,col="red",lty=2)

arrows(.6,.2,2.5,.4,col="blue")

text(3.2,.6,"y hat vector",col="blue")

arrows(2.5,.4,2.5,3,col="red")

text(3.4,1.7,"residual vector", col="red")