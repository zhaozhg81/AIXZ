library(lars)

data(diabetes)
par(mfrow=c(1,2))
attach(diabetes)
object <- lars(x,y)
plot(object)
object2 <- lars(x,y,type="lar")
plot(object2)
