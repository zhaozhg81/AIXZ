##Data set
## X: the number of hours studied 
## Y: the corresponding exam score for 16 students

df <- data.frame(hours=c(1, 1, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7, 8),
                 score=c(48, 78, 72, 70, 66, 92, 93, 75, 75, 80, 95, 97, 90, 96, 99, 99))


#fit simple linear regression model
model <- lm(score ~ hours, data = df)

#view summary of model
summary(model)

#create residual vs. fitted plot
plot(fitted(model), resid(model), xlab='Fitted Values', ylab='Residuals')

#add a horizontal line at 0 
abline(0,0)

#define weights to use
wt <- 1 / lm(abs(model$residuals) ~ model$fitted.values)$fitted.values^2

#perform weighted least squares regression
wls_model <- lm(score ~ hours, data = df, weights=wt)

#view summary of model
summary(wls_model)

