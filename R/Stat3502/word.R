word=read.table("data/3502/word.txt",header=T)

word.lm = lm(prop~time, data=word)
summary(word.lm)

par(mfrow=c(2,1))
plot(word$time, word$prop, pch=16, col='blue', ylim=c(0,1))
abline(word.lm, col='red', lwd=2)

plot( word.lm$fitted.values, word.lm$residuals )
abline(0,0, col='black')

word.tran.lm = lm(prop~log10(time), data=word)
summary( word.tran.lm )

plot( log10( word$time), word$prop, pch=16, col='blue', ylim=c(0,1))
abline( word.tran.lm, col='red', lwd=2)

plot(word.tran.lm$fitted.values, word.tran.lm$residuals)
abline(0,0,col='black')

qqnorm( word.tran.lm$residuals)
qqline(word.tran.lm$residuals, col = "red", lwd = 2)


ks.test(word.tran.lm$residuals/sd(word.tran.lm$residuals), 'pnorm')
shapiro.test(word.tran.lm$residuals)