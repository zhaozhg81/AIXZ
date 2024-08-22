library(HH)

#### Carbo example
data(cereals)
carbo <- cereals$carbo[ setdiff(1:dim(cereals)[1], 58) ]

alpha=0.05


mean(carbo)
LCL.t <- mean(carbo) - qt( alpha/2, 95, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )
UCL.t <- mean(carbo) + qt( alpha/2, 95, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )


LCL <- mean(carbo) - qnorm( alpha/2, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )
UCL <- mean(carbo) + qnorm( alpha/2, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )
