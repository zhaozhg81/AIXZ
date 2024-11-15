library(pls)

#make this example reproducible
set.seed(1)

head(mtcars)
?mtcars

round( cor(mtcars[,2:7]), digits=2 )

lm.model <- lm(hp~mpg+disp+drat+wt+qsec, data=mtcars)
summary(model)


pca.fit = prcomp(~mpg+disp+drat+wt+qsec,data=mtcars, scale=T)
screeplot(pca.fit, type="lines")


#fit PCR model
model <- pcr(hp~mpg+disp+drat+wt+qsec, data=mtcars, scale=TRUE, validation="CV")

#view summary of model fitting
summary(model)


#define training and testing sets
train <- mtcars[1:25, c("hp", "mpg", "disp", "drat", "wt", "qsec")]
y_test <- mtcars[26:nrow(mtcars), c("hp")]
test <- mtcars[26:nrow(mtcars), c("mpg", "disp", "drat", "wt", "qsec")]

## Use linear model
lm.model = lm(hp~mpg+disp+drat+wt+qsec, data = train)
lm_pred = predict( lm.model, test )
sqrt( mean((lm_pred-y_test)^2))

#use model to make predictions on a test set
model <- pcr(hp~mpg+disp+drat+wt+qsec, data = train, scale =TRUE, validation = "CV")
pcr_pred <- predict(model, test, ncomp = 2)

#calculate RMSE
sqrt(mean((pcr_pred - y_test)^2))