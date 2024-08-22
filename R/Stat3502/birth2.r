mydata=read.table("C:/Users/dongy/Desktop/linear regression/birthsmokers.txt",header=T)

#####################################################################
# compare confidence intervals from joint model v.s. seperate models

y=mydata$Wgt

x1=mydata$Gest

x2=as.numeric(mydata$Smoke)-1

data1=data.frame(y=y,x1=x1,x2=x2)

out=lm(y~., data=data1)

new_obs = data.frame(x1=38,x2=0)

predict(out,new_obs, interval = "confidence")

#        fit     lwr      upr
#   1 3048.237 2989.12 3107.355

# 95% confidence interval of mean birth weight of babys
# from non-smoking mothers with 38 week gestation is
# (2989.12 ,  3107.355)

new_obs = data.frame(x1=38,x2=1)

predict(out,new_obs, interval = "confidence")

#      fit      lwr      upr
#  1 2803.693 2740.599 2866.788

# 95% confidence interval of mean birth weight of babys
# from smoking mothers with 38 week gestation is
# (2740.599,  2866.788)

#################################################

n=length(y)

ind=(1:n)[x2==1]

data2=data.frame(y=y[ind],x1=x1[ind])

# create a data set that only has smokers

out2=lm(y~., data=data2)

new_obs = data.frame(x1=38)

predict(out2,new_obs, interval = "confidence")

#   fit      lwr      upr
#1 2808.528 2731.726 2885.331

# 95% confidence interval of mean birth weight of babys
# from smoking mothers with 38 week gestation is
# (2731.726, 2885.331)



#################################################


data3=data.frame(y=y[-ind],x1=x1[-ind])

# create a data set that only has non-smokers

out3=lm(y~., data=data3)

new_obs = data.frame(x1=38)

predict(out3,new_obs, interval = "confidence")

#     fit      lwr     upr
# 1 3047.724 2990.298 3105.15

# 95% confidence interval of mean birth weight of babys
# from non-smoking mothers with 38 week gestation is
# (2990.298 , 3105.15)


#Joint model has shorter confidence intervals for smokers
# (2740.599,  2866.788) from joint model v.s.  (2731.726, 2885.331) from seperate model

# length of confidence intervals for non-smokers are not much different
#  (2989.12 ,  3107.355) from joint model v.s.  (2990.298 , 3105.15) from seperate model


#####################################################

# we can also compare the standard errors of 
# estimated coefficient for x1 in all three models

(summary(out))$coefficients

(summary(out2))$coefficients

(summary(out3))$coefficients

# the standard error for beta1_hat is  
# 9.13, 14.11 and 11.97 respectively 