mydata=read.table("C:/Users/dongy/Desktop/linear regression/cement.txt",header=T)

head(mydata)

summary(lm(y~.,data=mydata))$coefficients

#from the p-values, x3 should be deleted (.896)

summary(lm(y~x1+x2+x4,data=mydata))$coefficients

# from the p-values x4 should be deleted (.2)

summary(lm(y~x1+x2,data=mydata))$coefficients

# both x1 and x2 have small p-values,this is the final model
