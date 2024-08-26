mydata=read.table("data/3502/bluegills.txt",header=T)
# change the directory to your own

str(mydata)

y=mydata$length
x1=mydata$age
x2=x1^2

myfit=lm(y~x1+x2)

ab=myfit$coefficients

print(round(ab,2))

myfunction=function(x){
  13.62+54.05*x-4.72*x^2
}

plot(x1,y,xlab="age",ylab="length", cex.lab=1.5,
     main="length=13.62+54.05 age-4.72 age squared")
plot(myfunction,min(x1),max(x1),add=T)
