pluto=read.table("data/3502/alphapluto.txt",header=T)

head(pluto)

pluto.lm = lm( alpha~pluto, data=pluto)
summary( pluto.lm )

plot(pluto$pluto, pluto$alpha )
abline(pluto.lm, col='red')

plot( pluto.lm$fitted.values, pluto.lm$residuals)
abline(0,0, col='red')