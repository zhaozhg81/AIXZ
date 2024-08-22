
par(mfrow = c(1, 2)) 

plot(1, type="n", main="?=",xlab="x", ylab="y", xlim=c(-1,2),ylim=c(-1,3))

points(1,2,pch=4)

text(1.3,2.2,"(1,2)")

points(0,0)

text(0,-.2,"(0,0)")

arrows(0, 0, 1, 2,col="red")

text(.3,1,"?",col="purple")

###############################################

plot(1, type="n", main=expression(paste("?=",sqrt(1^2+2^2))),xlab="x", ylab="y", xlim=c(-1,2),ylim=c(-1,3))

points(1,2,pch=4)

text(1.3,2.2,"(1,2)")

points(0,0)

text(0,-.2,"(0,0)")

arrows(0, 0, 1, 2,col="red")

segments(0, 0, 1, 0,lty=1)

segments(1, 0, 1, 2,lty=1)

text(1,-.2,"(1,0)")

text(.5,0.2,"1",col="purple")

text(1.2,1,"2",col="purple")

text(.3,1,"?",col="purple")