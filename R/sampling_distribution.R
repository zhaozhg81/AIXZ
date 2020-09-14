mu = 1
sigma = 1

no.rep.sampling = 1000

n.sample.size=100

X = array( rnorm( no.rep.sampling * n.sample.size, mu, sigma) , c(no.rep.sampling, n.sample.size) ) 

X.bar = apply( X, 1, mean )

