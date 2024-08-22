mydata=read.table("C:/Users/Yuexiao Dong/Desktop/cement.txt",header=T)

head(mydata)

summary(lm(y~x1,data=mydata))$coefficients

summary(lm(y~x2,data=mydata))$coefficients

summary(lm(y~x3,data=mydata))$coefficients

summary(lm(y~x4,data=mydata))$coefficients

# x4 will be added first according to smallest p-value (5.762318e-04)

summary(lm(y~x4+x1,data=mydata))$coefficients

summary(lm(y~x4+x2,data=mydata))$coefficients

summary(lm(y~x4+x3,data=mydata))$coefficients

# given x4 in the model, x1 (1.105281e-06) will be added next 

summary(lm(y~x4+x1+x2,data=mydata))$coefficients

summary(lm(y~x4+x1+x3,data=mydata))$coefficients

# given x4 and x1 in the model, x2 (5.168735e-02) will be added next

summary(lm(y~.,data=mydata))$coefficients

# given x4, x1, x2 in the model, x3 (0.50090110) will NOT be added

