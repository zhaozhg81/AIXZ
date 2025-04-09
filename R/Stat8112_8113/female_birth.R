## Plot the beta function

postscript("./Beta_distribution.eps",horizontal=FALSE)
x <- c(0:1000)/1000
plot( x, dbeta(x, 1, 1), 'l',col='red',ylim=c(0, 3))
points(x, dbeta(x, 2, 2), 'l', col='green')
points(x, dbeta(x, 5, 5), 'l', col='blue' )
points(x, dbeta(x, .5, 2), 'l', col='yellow')
points(x, dbeta(x, 0.5, 0.5), 'l',col='black')
dev.off()



## Set the prior
alpha <- 1
beta <- 1

## Posterior
set.seed(2)
N <- 1000000
theta.post <- rbeta( N, 437+alpha, 543+beta )

## postscript("theta_post_draw.eps",horizontal=FALSE)
hist(theta.post,br=100 )
## dev.off()

## postscript("theta_logit.eps",horizontal=FALSE)
hist(log(theta.post/(1-theta.post)),br=100 )
## dev.off()

## postscript("theta_odds.eps",horizontal=FALSE)
hist(theta.post/(1-theta.post),br=100 )
## dev.off()

## Inference of theta
median( theta.post )
quantile( theta.post, c(0.025, 0.975) )
mean( theta.post < 0.485 )


## Inference of logit(theta)
median( log( theta.post/(1-theta.post) ) )
quantile( log( theta.post/(1-theta.post) ), c(0.025, 0.975) )

## Inference of theta/(1-theta)
median( ( theta.post/(1-theta.post) ) )
quantile( ( theta.post/(1-theta.post) ), c(0.025, 0.975) )


median( log( theta.post/(1-theta.post) ) )
quantile( log( theta.post/(1-theta.post) ), c(0.025, 0.975) )

## Test if the true proportion of female births in the population of placenta previa births is lessthan 0.45.
post.prob <- mean( (theta.post)< 0.485 )

## True estimator
mean( log( theta.post/(1-theta.post) ) )
## Plug in estimator
log( mean( theta.post) /( 1- ( mean(theta.post) ) ) )



    ## Experiment
    n <- 100
    x <- 20

    theta.post <- rbeta(N, x+1, n-x+1)

    mean( theta.post/(1-theta.post))
    mean(theta.post)/( 1- mean(theta.post) )
