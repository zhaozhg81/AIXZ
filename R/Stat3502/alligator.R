alligator=read.table("data/3502/alligator.txt",header=T)

alligator.lm = lm(weight~length, data=alligator)

plot(alligator$length, alligator$weight,ylim=c(0,650))
abline(alligator.lm, col='red')