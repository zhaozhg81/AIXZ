
## Example car_maker
car.maker <- read.table("data/Car_Maker.csv", header=TRUE,sep=",")

car.lm <- lm( price ~ age + maker , data=car.maker )
summary( car.lm )

car.maker$maker <- as.factor(car.maker$maker )
car.lm <- lm( price ~ age + maker , data=car.maker )
summary( car.lm )



## Multiple comparison
car.pairwise <- glht( car.lm, linfct=mcp(maker="Tukey") )
summary( car.pairwise )


