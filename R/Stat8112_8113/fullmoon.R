## Example: Mental hospital admissions during full moons.

fm <- read.table("data/fullmoon.txt", header = TRUE)
adm <- fm$Admission

Month.char <-  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
month <- factor(fm$Month,levels = Month.char)

Moon.char <- c("Before","During","After")
moon <- factor(fm$Moon, levels = Moon.char)

fm2 <- data.frame(moon,month,adm)
names(fm2) <- c("Moon","Month","Admission")

library(ggplot2)
library(stats)

tapply(fm2$Admission,fm2$Moon,mean)
tapply(fm2$Admission,fm2$Month,mean)

qplot(fm2$Month,fm2$Admission, colour = fm2$Moon)

ggplot(fm2, aes(Month, Admission, color = Moon, group = Moon)) +   geom_point() + geom_line()

fit.fm <- lm(Admission ~ Moon + Month, data = fm2)
## Check the normality assumption
ks.test( fit.fm$residual/sqrt(var(fit.fm$residual)), 'pnorm' )

summary(fit.fm)
anova(fit.fm)

aov.fm <- aov(formula = Admission ~ Moon + Month, data = fm2, na.action =  na.exclude)
summary( aov.fm )
model.tables( aov.fm, type="mean" )
model.tables( aov.fm, type="effect" )
coef( aov.fm )
