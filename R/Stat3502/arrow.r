x1=2
y1=1
x2=1
y2=0
x0=0
y0=0


plot(1, type="n", xlab="x", ylab="y", xlim=c(-1,3),ylim=c(-1,3))

# Create arrow between the points
arrows(x0, y0, x1, y1)

arrows(x0, y0, x2, y2)

text(2.1,1,"a")

text(1.1,0,"b")