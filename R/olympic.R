olymp <- read.table("data/olympic.txt", header=TRUE)

plot( LongJump ~ Year, data=olymp)

longjump <- lm( LongJump ~ Year, data=olymp )
summary( longjump )


## Deletion diagnostics
p=1
cook.dist <- cooks.distance( longjump )
dotchart( pf( cook.dist, p+1, 20-(p+1) ) )


longjump.remove.outlier <- lm( LongJump ~ Year, data=olymp[ setdiff(c(1:23),c(1,16)), ] )
summary( longjump.remove.outlier )
cook.dist.remove <- cooks.distance( longjump.remove.outlier )
par( mfrow=c(2,1) )
dotchart( pf( cook.dist,p+1, 20-(p+1) ), xlim=c(0,1) )
dotchart( pf( cook.dist.remove, p+1, 18-(p+1)), xlim=c(0,1) )
