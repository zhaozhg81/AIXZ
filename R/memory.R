

# Example: Age and Memory

memory <- read.table("data/memory.txt", header = TRUE)
N <- dim(memory)[1]
a <- 2
b <- 5
n <- N/(a*b)

library(ggplot2)
library(stats)

memory$Process <- factor(memory$Process, levels = c("Counting","Rhyming","Adjective", "Imagery","Intentional"),  )
memory$Age <- factor(memory$Age, levels = c("Younger", "Older"))

attach(memory)
tapply(Words,Age,mean)
tapply(Words,Process,mean)
ggplot(memory, aes(Process, Words, color = Age, group = Age))+geom_point()


## Fit models
fit1 <- lm(Words ~ Age + Process, data = memory)
residual.pert <- rstudent( fit1 ) + rnorm(  length(fit1$fitted.values), 0, 0.001 )
ks.test( residual.pert, 'pnorm' )
layout( matrix( c(1,2,3,4), 2, 2) )
plot(fit1)
summary(fit1)
anova(fit1)

anov1 = aov(formula = Words ~ Age + Process, data = memory, na.action = na.exclude)
summary( anov1 )

model.tables(anov1, type="mean")
model.tables(anov1, type="effects")
coef(anov1)
dev.off()


## Check the existence of interaction
means = tapply( Words, list(Process, Age), mean)
## postscript(file='memory.eps', horizontal=FALSE)
plot(c(1:5), means[,1], type='o', col='blue', xlab="Counting, Rhyming, Adjective, Imagery, Intentional", ylab="Average words" )
points(c(1:5), means[,2], type='o', col='red')
legend(x=1,y=14,c("Young","Old"),col=c('blue','red'), lty=c(1,1) )
## dev.off()

## Anova with interaction
fit2 <- lm(Words ~ Age + Process + Age:Process, data = memory)
residual.pert.2 <- rstudent( fit2 ) + rnorm(  length(fit2$fitted.values), 0, 0.001 )
ks.test( residual.pert.2, 'pnorm' )
layout( matrix( c(1,2,3,4), 2, 2) )
plot(fit2)
anov2 = aov(formula = Words ~ Age + Process + Age:Process, data=memory, na.action=na.exclude )
summary( anov2 )

model.tables(anov2, type="mean")
model.tables(anov2, type="effects")
coef(anov2)
