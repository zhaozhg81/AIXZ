height <- read.csv("data/Galton.csv")

lm.height <- lm( CHILDC~PARENTC, data=height )
summary( lm.height )

