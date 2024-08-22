y=c(0,0.5,2,3.5)
x1=c(1,1,-1,-1)
x2=c(1,-1,1,-1)

(lm(y~x1))$coefficients

(lm(y~x2))$coefficients

(lm(y~x1+x2))$coefficients

cor(x1,x2)

a=c(1,1,1,1)

sum(a*x1)

sum(a*x2)
