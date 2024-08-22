#a x4+ b x3+ c  x2+ d x+  e=0

# 

x=seq(-1,-.5,length=1000)
y=18*x^5+51*x^4-34*x^3-178*x^2-60*x+75

plot(x,y,type="l")
abline(h=0)