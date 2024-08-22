n=50
set.seed(1)
x1=rnorm(n)
x2=x1+.1*rnorm(n)

y=x1-x2+.5*rnorm(n)

out0=lm(y~x1+x2)
vif(out0)

x=cbind(x1,x2)
covx=cov(x)
pc=x%*%(eigen(covx)$vectors)

pc1=pc[,1]
pc2=pc[,2]

par(mfrow=c(1, 2))

plot(x1,x2,cex.main=.7,main="correlation= 0.99")

plot(pc1,pc2,cex.main=.7,main="correlation = 0")

par(mfrow=c(1, 1))

cor(x1,x2)

cor(pc1,pc2)

out=lm(y~pc1+pc2)
vif(out)
