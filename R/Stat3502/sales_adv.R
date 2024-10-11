# Load necessary library
library(MASS)

# Set seed for reproducibility
set.seed(123)

# Context:
# A company is analyzing the relationship between its weekly advertising budget (in thousands of dollars)
# and the resulting weekly sales revenue (in thousands of dollars) over 100 weeks.
# They want to improve the linear regression model by using a Box-Cox transformation.


sale=read.table("data/3502/sale_adv.csv")


plot(sale$adv, sale$sales)
model <- lm(sales ~ adv, data = sale)
abline(model,col='red', lwd=2)

# View summary of the original model
summary(model)

# Plot diagnostic plots for residuals of the original model
par(mfrow = c(2, 2))
plot(model)
dev.off()


## 

model2 = lm(sales~adv+I(adv^2), data=sale)
summary(model2)

par(mfrow=c(2,2))
plot(model2)
dev.off()

datanew = data.frame(adv=8.8)
pred1= predict( model, newdata=datanew)
pred2= predict(model2, newdata=datanew)

plot(sale$adv,sale$sales)
abline(model,col='red',lwd=2)
points(sale$adv, model2$fitted.values, 'l', col='green', lwd=2)