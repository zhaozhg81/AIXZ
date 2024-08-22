par(mfrow = c(2, 2)) 

par(mar=c(2,2,2,2))

x1=2
y1=1
x2=1
y2=0
x0=0
y0=0


plot(1, type="n", xlab="x", ylab="y", xlim=c(-.5,2.5),ylim=c(-1,1.5))

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

text(2.1,1,"a")

text(1.1,0,"b")

plot(1, type="n", xlab="x", ylab="y", xlim=c(-.5,2.5),ylim=c(-1,1.5))

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

text(2.1,1,"a")

text(1.1,0,"b")

abline(h=0,lty=2,col="blue")



plot(1, type="n", xlab="x", ylab="y", xlim=c(-.5,2.5),ylim=c(-1,1.5))

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

text(2.1,1,"a")

text(1.1,0,"b")

abline(h=0,lty=2,col="blue")

abline(v=2,lty=2,col="red")



plot(1, type="n", xlab="x", ylab="y", xlim=c(-.5,2.5),ylim=c(-1,1.5))

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

text(2.1,1,"a")

text(1.1,0,"b")

abline(h=0,lty=2,col="blue")

abline(v=2,lty=2,col="red")

arrows(x0,y0,2,0,col="purple")