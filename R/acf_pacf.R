## PACF plot

T <- 1000
z1 <- array(0, T)
z2 <- array(0, T)

z1[1] <- rnorm(1)
for(t in 2:T )
  z1[t] <- -0.7 * z1[t-1] + rnorm(1)

z2[1:2] <- rnorm(2)
for(t in 3:T )
  z2[t] <- 0.5*z2[t-1] - 0.7 * z2[t-2] + rnorm(1)

postscript(file="../figure/pacf_1.eps", horizontal=FALSE)
plot(pacf(z1) )
dev.off()


postscript(file="../figure/pacf_2.eps", horizontal=FALSE )
plot(pacf(z2) )
dev.off()


theta=0.5
z3 <- array(0, T)
z3[1] <- rnorm(1)
eps.prev <- z3[1]
for(t in 2:T)
  {
    eps <- rnorm(1)
    z3[t] <- eps - 0.5 * eps.prev
    eps.prev <- eps
  }

z4 <- array(0, T)
z4[1] <- rnorm(1)
eps.prev <- z4[1]
for(t in 2:T)
  {
    eps <- rnorm(1)
    z4[t] <- eps + 0.5 * eps.prev
    eps.prev <- eps
  }

z5 <- array(0, T)
z5[1] <- rnorm(1)
z5[2] <- rnorm(1)

eps.prev.2 <- z5[1]
eps.prev.1 <- z5[2]
for(t in 3:T)
  {
    eps <- rnorm(1)
    z5[t] <- eps + 0.5 * eps.prev.1 - 0.3*eps.prev.2
    eps.prev.2 <- eps.prev.1
    eps.prev.1 <- eps
  }


postscript(file="../figure/acf_MA_pos.eps", horizontal=FALSE)
plot(pacf(z3) )
dev.off()

postscript(file="../figure/acf_MA_neg.eps", horizontal=FALSE )
plot(pacf(z4) )
dev.off()

postscript(file="../figure/acf_MA_2.eps", horizontal=FALSE )
plot(pacf(z5) )
dev.off()


