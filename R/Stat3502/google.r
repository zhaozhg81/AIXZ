library(zoo)


#tmp = read.table("C:/Users/dongy/Desktop/linear regression/google_stock.txt", header=F)
#head(tmp)

#tmp1=data.frame(time=as.factor(c(as.character(tmp[,1]),as.character(tmp[,3]),as.character(tmp[,5]))), price=c(tmp[,2],tmp[,4],tmp[,6]))

#write.table(tmp1, "google.txt")

google.ts=read.zoo("C:/Users/dongy/Desktop/linear regression/google.txt", format="%m/%d/%Y",
                      header=T)

plot(google.ts,xlab="",ylab="google stock price")  

##################

tmp = read.table("C:/Users/dongy/Desktop/linear regression/google.txt", header=T)
attach(tmp)

price.ts = ts(price)
pacf(price.ts,main="")

lag1price=lag(price.ts, -1)
plot(price.ts ~ lag1price, xy.labels=F)

lagdata = ts.intersect(price.ts, lag1price, dframe=T)
summary(lm(price.ts ~ lag1price, data=lagdata))

#########################################
res=lm(price.ts ~ lag1price, data=lagdata)$residuals

qqnorm(res)
shapiro.test(res)

pacf(res,main="residual")

