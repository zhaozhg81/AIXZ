x1=1
y1=0
x2=0
y2=1
x0=0
y0=0
x3=1
y3=1

par(mfrow = c(1, 1))  

plot(0, ylab="",xlab="", xaxt="n", yaxt="n", type="n",xlim=c(-1,2),ylim=c(-1,2))

title(ylab="y", xlab="x",mgp=c(2,2,0))

xtick=seq(-1, 2, by=1)
axis(side=1, at=xtick)

ytick=seq(-1, 2, by=1)
axis(side=2, at=ytick)

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

arrows(x0, y0, x3, y3)

text(0.5,-0.1,"a")

text(-.1,.5,"b")

text(0.5,0.6,"c")

text(-.1,-.1,"O",col="purple")

text(1.1,1.1,"D",col="purple")


######################################################################################################

par(mfrow = c(1, 2))   

plot(0, ylab="",xlab="", xaxt="n", yaxt="n", type="n",xlim=c(-1,2),ylim=c(-1,2))

title(main="a+b",ylab="y", xlab="x",mgp=c(2,2,0))

xtick=seq(-1, 2, by=1)
axis(side=1, at=xtick)

ytick=seq(-1, 2, by=1)
axis(side=2, at=ytick)

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

arrows(x0, y0, x3, y3)

text(0.5,-0.1,"a")

text(-.1,.5,"b")

text(0.5,0.6,"c")

text(-.1,-.1,"O",col="purple")

text(1.1,1.1,"D",col="purple")


arrows(x1, y1, x3, y3,col="red")
text(1.1,.5,"b",col="red")



#################################

plot(0, ylab="",xlab="", xaxt="n", yaxt="n", type="n",xlim=c(-1,2),ylim=c(-1,2))

title(main="b+a",ylab="y", xlab="x",mgp=c(2,2,0))

xtick=seq(-1, 2, by=1)
axis(side=1, at=xtick)

ytick=seq(-1, 2, by=1)
axis(side=2, at=ytick)

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

arrows(x0, y0, x3, y3)

text(0.5,-0.1,"a")

text(-.1,.5,"b")

text(0.5,0.6,"c")

text(-.1,-.1,"O",col="purple")

text(1.1,1.1,"D",col="purple")




arrows(x2, y2, x3, y3,col="blue")
text(.5,1.1,"a",col="blue")