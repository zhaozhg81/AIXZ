

#### Carbo example
data(cereals)
carbo <- cereals$carbo[ setdiff(1:dim(cereals)[1], 58) ]

mean(carbo)
LCL <- mean(carbo) - qnorm( alpha/2, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )
UCL <- mean(carbo) + qnorm( alpha/2, lower.tail=FALSE) * sqrt( var( carbo)/length(carbo ) )
