library(VGAM)
library(reshape2)


dat <- read.table("http://astro.temple.edu/~zhaozhg/Stat8113/data/highschool.csv",header=TRUE, sep=",")


## Preprocessing the dataset
res1 <- tapply( dat$prog=='academic', list( dat$ses, dat$write), sum )
res2 <- tapply( dat$prog=='general',  list( dat$ses, dat$write), sum )
res3 <- tapply( dat$prog=='vocation', list( dat$ses, dat$write), sum )
res1[ is.na(res1) ] <- 0
res2[ is.na(res2) ] <- 0
res3[ is.na(res3) ] <- 0
new.data <- expand.grid(ses=c("high","low","middle"),  write=sort(unique(dat$write)) )
new.data$academic <- c(res1)
new.data$general <- c(res2)
new.data$vocation <- c(res3)
## Remove zeros if three counts are zero.
remove.ind <- which( apply( new.data[,3:5], 1, sum )==0 )
new.data <- new.data[ -remove.ind, ] 


multi.model.para <- vglm( cbind(vocation, general, academic)~ ses + write, data=new.data, fam=multinomial(parallel=TRUE) )
multi.model.without.para <- vglm( cbind(vocation, general, academic)~ ses + write, data=new.data, fam=multinomial(parallel=FALSE) )
lrtest( multi.model.without.para, multi.model.para)


##
m0 <- vglm( cbind(vocation, general, academic)~ ses, data=new.data, fam=multinomial(parallel=TRUE) )
m1 <- vglm( cbind(vocation, general, academic)~ write, data=new.data, fam=multinomial(parallel=TRUE) )
lrtest( m0, multi.model.para )
lrtest( m1, multi.model.para )


prob <- predict( multi.model.without.para, type="response" )
pred.prob <- cbind( new.data[,1:2], prob )
pred.prob <- melt(pred.prob, id.vars = c("ses", "write"), value.name = "probability")
       
ggplot(pred.prob, aes(x = write, y = probability, colour = ses)) + geom_line() + facet_grid(variable ~ ., scales = "free")
