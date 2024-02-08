library(VGAM)
pneumo.df <- read.table("./data/pneumo.txt", header=T)
pneumo.df

attach(pneumo.df)

lodds <- function(x,y){log((x+.5)/(y+.5))}

multi.model <- vglm( cbind(normal,mild,severe)~ time, data=pneumo.df, fam=multinomial(parallel=TRUE~ time-1) )

plot( time, lodds(normal,severe))
plot( time, lodds(mild,severe) )



plot( log(time), lodds(normal,severe))
plot( log(time), lodds(mild,severe) )


multi.model <- vglm( cbind(normal,mild,severe)~ log(time), data=pneumo.df, fam=multinomial(parallel=TRUE~log(time)-1) )

## Fitted
base.fit.value <- fitted( multi.model )

pnewmo.base.normal <- base.fit.value[,1]
pnewmo.base.mild <- base.fit.value[,2]
pnewmo.base.severe <- base.fit.value[,3]
cbind(pnewmo.base.normal,pnewmo.base.mild,pnewmo.base.severe)

plot( log(time), lodds( normal, severe), xlab="", ylab="Base Logit")
points( log(time), log( pnewmo.base.normal/pnewmo.base.severe), 'l', col='red' )

plot( log(time), lodds( mild, severe), xlab="", ylab="Base Logit")
points( log(time), log( pnewmo.base.mild/pnewmo.base.severe), 'l', col='red' )

## Check the graph

r1.base <- pnewmo.base.normal
r2.base <- pnewmo.base.normal+pnewmo.base.mild

plot(log(time), lodds(normal, mild+severe), xlab="", ylab="1st CUM.LOGIT")
points( log(time), log(r1.base/(1-r1.base)), 'l', col='green')

plot(log(time), lodds(mild+normal, severe), xlab="Log Exposure", ylab="2nd Cum.Logit")
points( log(time), log(r2.base/(1-r2.base)), 'l', col='green')


clm.fit <- vglm( cbind( normal, mild, severe)~log(time), family=cumulative(parallel=TRUE~log(time)-1 ), data=pneumo.df)
summary( clm.fit)
fit.value <- fitted( clm.fit )
gamma.normal <- fit.value[,1]
gamma.mild <- fit.value[,1] + fit.value[,2]
gamma.severe <- fit.value[,1] + fit.value[,2] + fit.value[,3]
plot(log(pneumo.df[,1]), log( gamma.normal/(1-gamma.normal)),'l',col='red')
points(log(pneumo.df[,1]), log( gamma.mild/(1-gamma.mild)),'l',col='green')

## Fitted 
pnewmo.po.normal <- fit.value[,1]
pnewmo.po.mild <- fit.value[,2]
pnewmo.po.severe <- fit.value[,3]

r1 <- pnewmo.po.normal
r2 <- pnewmo.po.normal+pnewmo.po.mild



## Save the file
## postscript("plot_pneu_a.eps",horizontal=FALSE)
plot(log(time), lodds(normal, mild+severe), xlab="", ylab="1st CUM.LOGIT")
title("Cumulative logit")
points( log(time), log(r1/(1-r1)), 'l', col='red')
points( log(time), log(r1.base/(1-r1.base)), 'l', col='green')
legend('bottomleft', c('data','clm','blm'),lty=c(1,1,1),col=c('black','red','green'),cex=2 )
       
## dev.off()
## postscript("plot_pneu_b.eps",horizontal=FALSE)
plot(log(time), lodds(mild+normal, severe), xlab="Log Exposure", ylab="2nd Cum.Logit")
title("Cumulative logit")
points( log(time), log(r2/(1-r2)), 'l',col='red')
points( log(time), log(r2.base/(1-r2.base)), 'l', col='green')
legend('bottomleft', c('data','clm','blm'),lty=c(1,1,1),col=c('black','red','green'),cex=2 )

## dev.off()
## postscript("plot_pneu_c.eps",horizontal=FALSE)
plot( log(time), pnewmo.po.normal, 'l',col='green' ,ylim=c(0,1) )
points( log(time), pnewmo.po.mild, 'l',col='blue' )
points( log(time), pnewmo.po.severe, 'l',col='red' )
title("Fitted value")
legend( x=2.0, y=0.6, c("normal","mild","severe"), col=c("green","blue","red"), lty=c(1,1,1))

## dev.off()


## Continuation-ratio model analysis
crm.fit <- vglm( cbind( normal, mild, severe)~log(time), family=cratio(parallel=TRUE), data=pneumo.df)

## Fitted based on CRM 
pnewmo.crm.normal <- fitted( crm.fit )[,1]
pnewmo.crm.mild <- fitted(crm.fit)[,2]
pnewmo.crm.severe <- fitted(crm.fit)[,3]

r1 <- pnewmo.crm.normal
r2 <- pnewmo.crm.mild
r3 <- pnewmo.crm.severe
## Save the file
## postscript("plot_crm_pneu_a.eps",horizontal=FALSE)
plot(log(time), lodds(normal, mild+severe), xlab="", ylab="1st CUM.LOGIT")
title("Cumulative logit")
points( log(time), log(r1/(1-r1)), 'l', col='red')
## dev.off()

## postscript("plot_crm_pneu_b.eps",horizontal=FALSE)
plot(log(time), lodds(mild, severe), xlab="Log Exposure", ylab="2nd Cum.Logit")
title("Cumulative logit")
points( log(time), log(r2/r3), 'l',col='red')
## dev.off()


## postscript("plot_crm_pneu_c.eps",horizontal=FALSE)
plot( log(time), pnewmo.crm.normal, 'l',col='green' ,ylim=c(0,1) )
points( log(time), pnewmo.crm.mild, 'l',col='blue' )
points( log(time), pnewmo.crm.severe, 'l',col='red' )
title("Fitted value")
legend( x=2.0, y=0.6, c("normal","mild","severe"), col=c("green","blue","red"), lty=c(1,1,1))

## dev.off()

## likelihood ratio testß
m0 <-  vglm( cbind( normal, mild, severe)~1, family=cratio(parallel=TRUE), data=pneumo.df)
lrtest( m0, crm.fit )
