library(lars)
library(glmnet)


data(diabetes)

par(mfrow=c(1,2))
attach(diabetes)

object <- lars(x,y)

plot(object)

object2 <- lars(x,y,type="lar")
plot(object2)

## K-fold cross-validation
cv_model = cv.glmnet( x, y, alpha=1, nfolds=10 )
best_lambda = cv_model$lambda.min

best_model = glmnet(x,y, alpha=1, lambda=best_lambda)

coef( best_model )
