n <- 500
x <- c(50, 156, 294)

p.hat <- x/n
theta.hat <- (2*x[1]+x[2])/2/n
p0.hat <- c( theta.hat^2, 2*theta.hat*(1-theta.hat), (1-theta.hat)^2 )

test.stat <- 2*sum( x*log(p.hat/p0.hat) )
