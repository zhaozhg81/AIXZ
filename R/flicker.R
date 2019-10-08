
# Example: Eye flicker frequency vs. eye color

flicker <- read.table("http://astro.temple.edu/~zhaozhg/Stat8113/data/flicker.txt",header=TRUE)

tapply(flicker$Flicker,flicker$Colour,mean)

par(mar = c(4.5,4.5,1,1))
plot(Flicker~Colour, data = flicker, xlab="Eye color", ylab="Flicker frequency")


fit.flicker <- lm( Flicker ~ Colour, data = flicker)
summary(fit.flicker)
anova(fit.flicker)
## Check the normality assumption
ks.test( fit.flicker$residual/sqrt(var(fit.flicker$residual)), 'pnorm' )

ANOVA.flicker <- aov(formula = Flicker ~ Colour, data = flicker, na.action =      na.exclude)
summary(ANOVA.flicker)

model.tables(ANOVA.flicker, type="mean")
model.tables(ANOVA.flicker, type="effects")
coef(ANOVA.flicker)



flicker.pairwise <- glht( fit.flicker, linfct=mcp(Colour="Tukey") )
summary(flicker.pairwise)
