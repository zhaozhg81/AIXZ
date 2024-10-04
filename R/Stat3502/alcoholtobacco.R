tabacco=read.table("data/3502/alcoholtobacco.txt",header=T)
head(tabacco)

tabacco.lm = lm( Alcohol ~ Tobacco, data=tabacco)

plot(tabacco$Tobacco, tabacco$Alcohol)
abline( tabacco.lm, col='red')

tabacco.lm.2 = lm( Alcohol ~ Tobacco, data=tabacco[1:10,])

plot(tabacco$Tobacco, tabacco$Alcohol)
abline( tabacco.lm.2, col='red')

summary(tabacco.lm)
summary( tabacco.lm.2)