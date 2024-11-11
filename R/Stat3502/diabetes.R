library(lars)
library(glmnet)


data(diabetes)

par(mfrow=c(1,2))
attach(diabetes)

object <- lars(diabetes$x,diabetes$y)

plot(object)

object2 <- lars( diabetes$x,diabetes$y,type="lar")
plot(object2)

## K-fold cross-validation
cv_model = cv.glmnet( diabetes$x, diabetes$y, alpha=1, nfolds=10 )
best_lambda = cv_model$lambda.min

best_model = glmnet(diabetes$x,diabetes$y, alpha=1, lambda=best_lambda)

coef( best_model )
