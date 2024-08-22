mydata=read.table("C:/Users/dongy/Desktop/linear regression/birthsmokers.txt",header=T)

#####################################################################
# do we need two indicator variables for smoking status?

y=mydata$Wgt

x1=mydata$Gest

x2=as.numeric(mydata$Smoke)-1

head(x2)

# check that smoking is coded as x2=1 and non-smoking is coded as x2=0

x3=2-as.numeric(mydata$Smoke)

head(x3)

# check that smoking is coded as x3=0 and non-smoking is coded as x3=1

out=lm(y~x1+x2+x3)

summary(out)

# note that x3 is dropped out of the analysis

#################################################################################
# what happens if we still use one indictor, but coded as (-1,1) instead of (0,1)?

y=mydata$Wgt

x1=mydata$Gest

x4=2*(as.numeric(mydata$Smoke)-1.5)

head(x4)

# check that smoking is coded as x4=1 and non-smoking is coded as x4=-1

out2=lm(y~x1+x4)

summary(out2)$coefficients

# recall that smoking is coded as x2=1 and non-smoking is coded as x2=0 previously

out1=lm(y~x1+x2)

summary(out1)$coefficients

# note the coefficients for x1 is the same in the two estimated models

