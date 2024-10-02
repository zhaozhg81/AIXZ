skincancer=read.table("data/3502/skincancer.txt",header=T)

skincancer.lm = lm(Mort~Lat, data=skincancer)

plot(skincancer$Lat, skincancer$Mort)
abline(skincancer.lm, col='red')