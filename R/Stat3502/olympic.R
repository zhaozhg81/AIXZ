olymp <- read.table("data/olympic.txt", header=TRUE)

plot( LongJump ~ Year, data=olymp)

longjump <- lm( LongJump ~ Year, data=olymp )
summary( longjump )



## 
y=olymp$LongJump
x=olymp$Year

n=length(y)

newx=cbind(1,x)

h=newx%*%solve(t(newx)%*%newx)%*%t(newx)

lev=diag(h)

yhat=h%*%y

res=y-yhat

mse=sum(res^2)/(n-2)

r_res=res/sqrt(mse*(1-lev))

t_res=r_res*sqrt((n-2-1)/(n-2-r_res^2))


## Deletion diagnostics
cook.dist <- cooks.distance( longjump )


longjump.remove.outlier <- lm( LongJump ~ Year, data=olymp[ setdiff(c(1:23),c(1,16)), ] )
summary( longjump.remove.outlier )
cook.dist.remove <- cooks.distance( longjump.remove.outlier )


