options(rgl.useNULL=TRUE)
library(Rdimtools)

hts=read.csv("Data Files/PM5_95_1_3.3cmpermin-Ying.csv")
sample.ind = sample(c(1:58240), 5000)
sample.ind=c(1:58240)

X= scale( hts[sample.ind,3:8] )
Y = hts[sample.ind,9]
res = do.sir(X, Y, ndim=2)

index = as.matrix( X)%*% res$projection

par(mfrow=c(2,2))
plot(index[,1], log(Y))
plot(index[,2], log(Y))

result = data.frame( Y=log(Y), index1=index[,1], index2=index[,2])
result$lowCur = 1*(result$Y < 4)
tmp = result[ which(result$lowCur ==1), ]
plot(tmp$index1, tmp$Y)
plot(tmp$index2, tmp$Y)

## Found a few threshold 
## index1 < -2, 0.5< index2 < 1, correpsonding to the left part of low current
tmp[ which(tmp$index1 < -2),]
## -1< index1 < 0, 1< index2 < 1.5 or -1.5 < index2 < -1
tmp[ which( (tmp$index1 > -1)&(tmp$index1 < 0)), ]
## index1 >2, -1.5 < index2 < -1
tmp[ which( (tmp$index1 > 2)&(tmp$index2 > -1.5 ) &(tmp$index2 < -1)), ]


## Test on the whole set
result$Y[ which( result$index1 < -1.5)]
result$Y[ which( (tmp$index1 > -1)&(tmp$index1 < 0) &
                   ( abs(tmp$index2)>1 )&(abs(tmp$index2)<1.5))]
result$Y[ which( (tmp$index1 > 2)&
                   ( tmp$index2 > -1.5 )&( tmp$index2 < -1))]