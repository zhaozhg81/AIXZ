library(scatterplot3d) # This library will allow us to draw 3d plot

# Input data
x1 <- c(1.9,0.8,1.1,0.1,-0.1,4.4,4.6,1.6,5.5,3.4)
x2 <- c(66, 62, 64, 61, 63, 70, 68, 62, 68, 66)
y <- c(0.7,-1.0,-0.2,-1.2,-0.1,3.4,0.0,0.8,3.7,2.0)
dataset = cbind.data.frame(x1,x2,y)
scatterplot3d(x1,x2,y)

# Use optimization to find regression coefficients
# Same as fminsearch in matlab
ls <- function(dataset, par) 
{with(dataset, sum((y-par[1]-par[2]*x1-par[3]*x2)^2))}
result <- optim(par=c(0,0,0),ls,data=dataset) 
# starting values for the parameters to be optimised (0,0,0)
coef <- result$par
coef

# fit a linear model using lm model and draw a regression plane
#plot3d <- scatterplot3d(x1,x2,y, type="h", highlight.3d=TRUE,angle=55, scale.y=0.7, pch=16)
plot3d <- scatterplot3d(x1,x2,y,
                        angle=55, scale.y=0.7, pch=16, color ="red", main ="Regression Plane")
my.lm<- lm(y ~ x1 + x2,data=dataset)
plot3d$plane3d(my.lm, lty.box = "solid")

y_hat=my.lm$fitted.values

plot3d$points3d(x1,x2,y_hat,col="blue", type="h", pch=16)