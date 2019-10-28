library(fields)


wine <- read.table("data/wine.data",sep=",")

## Pairwise scatterplot
pairs(~., data=wine[,2:14])

## Correlation
wine.cor <- cor( wine[,2:14] )
breaks <- seq(from=-0.9,to=0.9,length=65)
hmcols <- tim.colors(64)
image.plot(x=1:13, y=1:13, z = wine.cor, zlim = c(-0.9,0.9),col=tim.colors(64),breaks=breaks,
           xlab="pariwise correlation",ylab="", cex.lab=1.5)

## Scatter plot.
plot(wine$V4, wine$V5)
text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")


stand.concent <- as.matrix(scale(wine[2:14])) # standardise the variables
wine.pca <- princomp(stand.concent)                 # do a PCA
summary( wine.pca )

## Use the matrix to calculate the principle component analysis
S <- t(stand.concent)%*%stand.concent/177
eigen(S)$vectors[,1]

## postscript("wine_scree.eps", horizontal=FALSE)
screeplot(wine.pca, type="lines")
## dev.off()

y <- array(0, c(178, 3) )
y[1:59,1] <- 1
y[60:130,2] <- 1
y[131:178,3] <- 1
colnames(y)= c("cult1", "cult2", "cult3")
full.data <- data.frame(y, stand.concent)

library(VGAM)
base.logit <- vglm( cbind(cult1, cult2, cult3) ~ .-cult1-cult2-cult3 , data=full.data, fam=multinomial(parallel=TRUE) )
summary( base.logit)
AIC( base.logit )
lrtest( base.logit )

pca.data <- data.frame(y, wine.pca$scores[,1:5] )
base.logit.pca <- vglm( cbind(cult1, cult2, cult3) ~ Comp.1 + Comp.2  , data=pca.data, fam=multinomial(parallel=TRUE) )
summary(base.logit.pca)
AIC(base.logit.pca)

## postscript("wine_scatter_pca.eps",horizontal=FALSE)
plot(wine.pca$score[,1],wine.pca$score[,2]) # make a scatterplot
text(wine.pca$score[,1],wine.pca$score[,2], wine$V1, cex=0.7, pos=4, col="red") # add labels
## dev.off()

#### LDA
library(MASS)
wine.lda <- lda(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + V14, data=wine)
wine.lda2 <- lda(V1 ~ .-V1, data=wine )
wine.lda.values <- predict(wine.lda, wine[2:14])
wine.lda.values$x

## A stacked Histogram of the LDA values
## postscript("stack_hist_lda_1.eps", horizontal=FALSE)
ldahist(data = wine.lda.values$x[,1], g=wine$V1)
## dev.off()
## postscript("stack_hist_lda_2.eps", horizontal=FALSE)
ldahist(data = wine.lda.values$x[,2], g=wine$V1)
## dev.off()

## postscript("wine_lda_scatterplot.eps",horizontal=FALSE)
plot(wine.lda.values$x[,1],wine.lda.values$x[,2]) # make a scatterplot
text(wine.lda.values$x[,1],wine.lda.values$x[,2],wine$V1,cex=0.7,pos=4,col="red") # add labels
## dev.off()
