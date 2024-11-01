
library(MASS)
library(olsrr)

mydata=read.table("data/3502/cement.txt",header=T)

head(mydata)

attach(mydata)

variables <- colnames(mydata)[2:ncol(mydata)]
formulas <- list()
for (i in seq_along(variables)) {
  tmp <- combn(variables, i)
  tmp <- apply(tmp, 2, paste, collapse="+")
  tmp <- paste0("y~", tmp)
  formulas[[i]] <- tmp
}
formulas <- unlist(formulas)
formulas <- sapply(formulas, as.formula)

models <- lapply(formulas, lm, data=mydata)

cor( mydata )
pairs(~x1+x2+x3+x4, data=mydata)

ols_mallows_cp( models[1], models[15])