y1 <- -1
y2 <- 5
rho <- 0.5
set.seed(2)


theta.post1 <- array(0, c(2, 10) )
theta.post1[,1] <- c(3,-2)

postscript("../figure/gibbs_1.eps",horizontal=FALSE)
plot(theta.post1[1,1], theta.post1[2,1], xlim=c(-5, 5), ylim=c(-5, 10) )
for(i in 2:10)
  {
    theta.post1[ 1,i ] <- rnorm( 1, y1 + rho* (theta.post1[2, i-1] - y2 ), sqrt(1-rho^2) )
    theta.post1[ 2,i ] <- rnorm( 1, y2 + rho*(theta.post1[1,i] - y1), sqrt(1-rho^2) )
  }
for( i in 1:9)
  {
    points( c(theta.post1[1,i], theta.post1[1,i+1]), c(theta.post1[2,i], theta.post1[2,i]), 'l', col='red')
    points( c(theta.post1[1,i+1], theta.post1[1,i+1]), c(theta.post1[2,i], theta.post1[2,i+1]), 'l', col='red')
  }

## Change initial point 2
theta.post2 <- array(0, c(2, 10) )
theta.post2[,1] <- c(-2,-2.5)
for(i in 2:10)
  {
    theta.post2[ 1,i ] <- rnorm( 1, y1 + rho* (theta.post2[2, i-1] - y2 ), sqrt(1-rho^2) )
    theta.post2[ 2,i ] <- rnorm( 1, y2 + rho*(theta.post2[1,i] - y1), sqrt(1-rho^2) )
  }
points(theta.post2[1,1], theta.post2[2,1])
for( i in 1:9 )
  {
    points( c(theta.post2[1,i], theta.post2[1,i+1]), c(theta.post2[2,i], theta.post2[2,i]), 'l', col='green')
    points( c(theta.post2[1,i+1], theta.post2[1,i+1]), c(theta.post2[2,i], theta.post2[2,i+1]), 'l', col='green')
  }

## Change initial point 3
theta.post3 <- array(0, c(2, 10) )
theta.post3[,1] <- c(3,6)
for(i in 2:10)
  {
    theta.post3[ 1,i ] <- rnorm( 1, y1 + rho* (theta.post3[2, i-1] - y2 ), sqrt(1-rho^2) )
    theta.post3[ 2,i ] <- rnorm( 1, y2 + rho*(theta.post3[1,i] - y1), sqrt(1-rho^2) )
  }
points(theta.post3[1,1], theta.post3[2,1])
for( i in 1:9)
  {
    points( c(theta.post3[1,i], theta.post3[1,i+1]), c(theta.post3[2,i], theta.post3[2,i]), 'l', col='blue')
    points( c(theta.post3[1,i+1], theta.post3[1,i+1]), c(theta.post3[2,i], theta.post3[2,i+1]), 'l', col='blue')
  }

## Change initial point 4
theta.post3 <- array(0, c(2, 10) )
theta.post3[,1] <- c(-2.5,5.5)
for(i in 2:10)
  {
    theta.post3[ 1,i ] <- rnorm( 1, y1 + rho* (theta.post3[2, i-1] - y2 ), sqrt(1-rho^2) )
    theta.post3[ 2,i ] <- rnorm( 1, y2 + rho*(theta.post3[1,i] - y1), sqrt(1-rho^2) )
  }
points(theta.post3[1,1], theta.post3[2,1])
for( i in 1:9)
  {
    points( c(theta.post3[1,i], theta.post3[1,i+1]), c(theta.post3[2,i], theta.post3[2,i]), 'l', col='cyan')
    points( c(theta.post3[1,i+1], theta.post3[1,i+1]), c(theta.post3[2,i], theta.post3[2,i+1]), 'l', col='cyan')
  }

dev.off()


## ############################################
## ############################################
## ############################################
## ############################################
## ############################################
## Real Gibbs sampler\
itr <- 1000
theta.post <- array(0, c(2, itr) )
theta.post[,1] <- c(3,-2)
for(i in 2:itr)
  {
    theta.post[ 1,i ] <- rnorm( 1, y1 + rho* (theta.post[2, i-1] - y2 ), sqrt(1-rho^2) )
    theta.post[ 2,i ] <- rnorm( 1, y2 + rho*(theta.post[1,i] - y1), sqrt(1-rho^2) )
  }

postscript("../figure/gibbs_2.eps",horizontal=FALSE)
plot(theta.post[1,1], theta.post[2,1], xlim=c(-5, 5), ylim=c(-5, 10) )
for( i in 1:(itr-1))
  {
    points( c(theta.post[1,i], theta.post[1,i+1]), c(theta.post[2,i], theta.post[2,i]), 'l', col='black')
    points( c(theta.post[1,i+1], theta.post[1,i+1]), c(theta.post[2,i], theta.post[2,i+1]), 'l', col='black')
  }

dev.off()

postscript("../figure/gibbs_3.eps",horizontal=FALSE)
plot( theta.post[1,], theta.post[2,])
dev.off()

postscript("../figure/gibbs_4.eps",horizontal=FALSE)
plot( c(1:itr), theta.post[1,])
dev.off()

burnin <- 1000
thin <- 10
theta.post.full <- array(0, c(2, burnin+thin*itr) )
theta.post.full[,1] <- c(3,-2)
for(i in 2:(burnin+thin*itr))
  {
    theta.post.full[ 1,i ] <- rnorm( 1, y1 + rho* (theta.post.full[2, i-1] - y2 ), sqrt(1-rho^2) )
    theta.post.full[ 2,i ] <- rnorm( 1, y2 + rho*(theta.post.full[1,i] - y1), sqrt(1-rho^2) )
  }
theta.post <- array(0, c(2, itr) )
ind <- burnin+1+thin*c(0:(itr-1))
theta.post <- theta.post.full[,ind]
