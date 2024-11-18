library(pls)

#make this example reproducible
set.seed(1)

head(mtcars)
?mtcars

round( cor(mtcars[,2:7]), digits=2 )

pairs(~disp+drat+wt+hp+qsec,data=mtcars )


lm.model <- lm(mpg~hp+disp+drat+wt+qsec, data=mtcars)
summary(lm.model)


pca.fit = prcomp(~hp+disp+drat+wt+qsec,data=mtcars, scale=T)
summary( pca.fit )

## Total variance 
sum( unlist( summary(pca.fit)[1] )^2 )

screeplot(pca.fit, type="lines")


#fit PCR model
model <- pcr(mpg~hp+disp+drat+wt+qsec, data=mtcars, scale=TRUE, validation="LOO")
t(model$loadings[,])%*%( model$loadings[,])


#view summary of model fitting
summary(model)
model$validation

###############################################
###############################################
###############################################
###############################################
## Ridge Regression

# Getting the independent variable
x_var <- data.matrix(mtcars[, c("hp", "wt", "drat","disp","qsec")])
# Getting the dependent variable
y_var <- mtcars[, "mpg"]
# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.1)
# Using glmnet function to build the ridge regression in r
fit <- glmnet(x_var, y_var, alpha = 0, lambda  = lambda_seq)
# Checking the model
summary(fit)

# Using cross validation glmnet
ridge_cv <- cv.glmnet(x_var, y_var, alpha = 0)
# Best lambda value
best_lambda <- ridge_cv$lambda.min
best_lambda

best_ridge <- glmnet(x_var, y_var, alpha = 0, lambda = best_lambda)


## Check the prediction performance
#define training and testing sets
pred.lm = array(0, nrow(mtcars))
pred.ridge = array(0, nrow(mtcars))

for(i in 1:nrow(mtcars))
{
  train <- mtcars[setdiff(c(1:nrow(mtcars)), i), c("hp", "mpg", "disp", "drat", "wt", "qsec")]
  y_test <- mtcars[i, c("mpg")]
  test <- mtcars[i, c("hp", "disp", "drat", "wt", "qsec")]

  ## Use linear model
  lm.model = lm(mpg~hp+disp+drat+wt+qsec, data = train)
  lm_pred = predict( lm.model, test )
  pred.lm[i] = (lm_pred-y_test)^2

  #use the ridge regression
  # Using cross validation glmnet
  ridge_cv <- cv.glmnet(x=data.matrix(train[, c("hp", "disp", "drat","wt","qsec")]), y=train[, "mpg"], alpha = 0)
  # Best lambda value
  best_lambda <- ridge_cv$lambda.min

  best_ridge <- glmnet(x=data.matrix(train[, c("hp", "disp", "drat","wt","qsec")]), y=train[, "mpg"], alpha = 0, lambda = best_lambda)
  
  ridge_pred= predict( best_ridge, s=best_lambda, newx=test)
  
  pred.ridge[i] = (ridge_pred - y_test)^2
}