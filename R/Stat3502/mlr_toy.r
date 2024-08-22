y=c(0,.5,2,3.5)
x1=c(1,1,-1,-1)
x2=c(1,-1,1,-1)

out=lm(y~x1+x2)
summary(out)

newx=cbind(1,x1,x2)
newy=as.matrix(y)

beta_hat=solve(t(newx)%*%newx)%*%t(newx)%*%newy

y_hat=newx%*%beta_hat

error=y-y_hat

sse=sum(error^2)
mse=sse/(length(y)-3)

se_beta_hat=sqrt(mse*diag(solve(t(newx)%*%newx)))

t_value=beta_hat/se_beta_hat

p_value=2*(1-pt(abs(t_value),df=length(y)-3))

list(Estimate=beta_hat,SE=se_beta_hat,t=t_value,p=p_value)

#####################################

out0=lm(y~1)
summary(out0)

anova(out0,out)

sst=sum((y-mean(y))^2)
F_value=((sst-sse)/2)/mse
F_value
1-pf(F_value,df1=2,df2=1)

######################################

out1=lm(y~x1)
anova(out1,out)

######################################

out2=lm(y~x2)
anova(out2,out)

