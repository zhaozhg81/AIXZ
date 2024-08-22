library( ggplot2 )
library( lme4 )
library( dplyr )
library( lmtest )

## Load the data
d <- read.csv('./data/politeness_data.csv')
str(d)

## Change the name of the data
names(d)[names(d)=="attitude"] <- "condition"
names(d)[names(d)=="frequency"] <- "pitch"

## visualize the data
theme_set(theme_bw(base_size = 18))
qplot(condition, pitch, facets = . ~ subject, 
      colour = subject, geom = "boxplot", data = d)

## Linear regression model
lm.fit <- lm( pitch ~ gender + condition, data= d )
plot( lm.fit )

pitch_bysubj = with(d, aggregate(pitch ~ subject, FUN = "mean"))
## Model with the intercepts as the only random effect
res1 <- lmer(pitch ~ (1|subject), data= d )
coef(res1)$subject[1]
summary( res1 )

## Mixed effects
d_bycond = na.omit(d) %>%   group_by(gender, condition) %>%   summarise(mean_pitch = mean(pitch))
ggplot(d_bycond, aes(x=condition, y=mean_pitch, colour=gender, group=gender)) +  geom_line(size=2) + geom_point(size=5, shape=21, fill="white")


res2 = lmer(pitch ~ condition + gender + (1|subject), data=d)
summary( res2 )
logLikelihood = logLik(res2)
deviance = -2*logLikelihood[1]
AIC( res2 )


## Random slopes
res3 = lmer(pitch ~ condition + gender + (1 + condition | subject), REML = TRUE, data = d)
summary(res3)
coef(res3)
anova(res2, res3, refit=FALSE)
AIC( res3)


## Model comparison
res4 = lmer(pitch ~ gender + (1 | subject), REML = FALSE, data = d)
res4b = lmer(pitch ~ condition + gender + (1 | subject), REML = FALSE, data = d)
anova(res4, res4b, refit=FALSE)
lrtest( res4, res4b )
