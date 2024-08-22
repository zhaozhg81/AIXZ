n=40

set.seed(1)

x=runif(n)
y=rep(0,n)

pi=exp(-2+4*x)/(1+exp(-2+4*x))

for(i in 1:n){
  tmp=pi[i]
  y[i]=rbinom(n=1,size=1,prob=tmp)
}

plot(x,y,xlim=c(-.1,1.1),ylim=c(-.5,1.5))

out=lm(y~x)
ab=out$coefficients

abline(ab[1],ab[2])


mydata=data.frame(x=x,y=y)

#write.table(mydata, "toy.txt")