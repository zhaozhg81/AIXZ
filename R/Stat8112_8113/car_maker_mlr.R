
## Example car_maker
car.maker <- read.table("data/Car_Maker.csv", header=TRUE,sep=",")
car.maker

car.lm <- lm( price ~ age + maker , data=car.maker )
summary( car.lm )

car.maker$maker <- as.factor(car.maker$maker )

reduced.lm <- lm(price ~ age, data=car.maker ) 
car.lm <- lm( price ~ age + maker , data=car.maker )
summary( car.lm )

anova( car.lm )
anova( reduced.lm, car.lm ) 

library(multcomp)
## Multiple comparison
car.pairwise <- glht( car.lm, linfct=mcp(maker="Tukey") )
summary( car.pairwise )


