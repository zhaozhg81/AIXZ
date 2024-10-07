skincancer=read.table("data/3502/skincancer.txt",header=T)

skincancer.lm = lm(Mort~Lat, data=skincancer)

plot(skincancer$Lat, skincancer$Mort)
abline(skincancer.lm, col='red')

hist( skincancer.lm$residuals/sd(skincancer.lm$residuals) )
qqnorm( skincancer.lm$residuals)

ks.test( skincancer.lm$residuals/sd(skincancer.lm$residuals), 'pnorm')

shapiro.test( skincancer.lm$residuals )