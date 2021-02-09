require(foreign)
require(ggplot2)
require(MASS)


dat <- read.dta("./data/nb_data.dta")
dat$prog <- factor( dat$prog, levels=1:3, labels=c("General", "Academic", "Vocational" ) )
ggplot(dat, aes(daysabs, fill = prog)) + geom_histogram(binwidth = 1) + facet_grid(prog ~ ., margins = TRUE, scales = "free")


## Normal model
attend.lm.2 <- lm( daysabs ~ math + prog, data= dat )
deviance( attend.lm.2 )
attend.lm <- glm( daysabs~ math + prog, data= dat, family=gaussian )
plot( predict( attend.lm), residuals( attend.lm), col='black')

## Poisson model
attend.pois <- glm( daysabs ~ math + prog, data= dat, family=poisson(link="log" ) )
summary( attend.pois )

points( predict( attend.pois) , residuals( attend.pois ), col='green' )

## Negative binomial model
attend.nb <- glm.nb( daysabs ~ math + prog, data= dat, link=log)
summary( attend.nb )
points( predict(attend.nb), residuals( attend.nb), col='red') 

## Is prog significant?
m2 <- update( attend.nb, .~. - prog )
anova(attend.nb, m2 )


## Can we compare two models poisson vs negative binomial? Yes.
lrtest( attend.pois, attend.nb )


## prediction
newdata1 <- data.frame(  math = rep(seq(from = min(dat$math), to = max(dat$math), length.out = 100), 3),  prog = factor(rep(1:3, each = 100), levels = 1:3, labels =  levels(dat$prog)))

newdata1 <- cbind(newdata1, predict(attend.nb, newdata1, type = "link", se.fit=TRUE))
newdata1 <- within(newdata1, {
  DaysAbsent <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)
})


ggplot( newdata1, aes( math, DaysAbsent )) +
  geom_line(aes(colour = prog), size = 2) +
  labs(x = "Math Score", y = "Predicted Days Absent")



## zero inflated poisson
zobs <- dat$daysabs == 0
zpoi <- dpois(0,exp(predict(attend.pois)))
c(obs=mean(zobs), poi=mean(zpoi))

attend.zip <- zeroinfl(daysabs~ math+prog, data=dat)

summary(attend.zip)