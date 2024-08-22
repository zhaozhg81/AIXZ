## Issue on p-values

set.seed(2)

mu = 1
n = 100
x_1=rnorm(n, mu, 2)
p.value = t.test(x_1)$p.value
print(paste("mu=", mu, "; n=",n,"; p-value=",p.value, sep=""))


mu = 0.1
n = 100
x_2=rnorm(n, mu, 2)
p.value = t.test(x_2)$p.value
print(paste("mu=", mu, "; n=",n,"; p-value=",p.value, sep=""))

mu = 0.1
n = 1000
x_3=rnorm(n, mu, 2)
p.value = t.test(x_3)$p.value
print(paste("mu=", mu, "; n=",n,"; p-value=",p.value, sep=""))

mu = 0.01
n = 1000
x_4=rnorm(n, mu, 2)
p.value = t.test(x_4)$p.value
print(paste("mu=", mu, "; n=",n,"; p-value=",p.value, sep=""))

mu = 0.01
n = 10000
x_5=rnorm(n, mu, 2)
p.value = t.test(x_5)$p.value
print(paste("mu=", mu, "; n=",n,"; p-value=",p.value, sep=""))

mu = 0.01
n = 100000
x_6=rnorm(n, mu, 2)
p.value = t.test(x_6)$p.value
print(paste("mu=", mu, "; n=",n,"; p-value=",p.value, sep=""))

mu = 0.01
n = 1000000
x_7=rnorm(n, mu, 2)
p.value = t.test(x_7)$p.value
print(paste("mu=", mu, "; n=",n,"; p-value=",p.value, sep=""))

## Are we testing the hypothesis that mu >0 or testing whether the sample size is large enough?

t.test(x_1)
t.test(x_7)
