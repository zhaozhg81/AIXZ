
library(MASS)

mydata=read.table("data/3502/cement.txt",header=T)

head(mydata)

##########################################

model.full=lm(y~.,data=mydata)

step=stepAIC(model.full, direction="backward")

##########################################

model.null=lm(y~1,data=mydata)

step=stepAIC(out, direction="forward",scope = list(lower = model.null,
                                                   upper = model.full))

###############################################

step=stepAIC(model.full, direction="both")

step=stepAIC(model.null, direction="both",scope = list(lower = model.null,
                                                       upper = model.full))
