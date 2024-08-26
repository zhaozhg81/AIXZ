
n = 2000
prob.row = runif(5)
prob.col = runif(5)
prob.row = prob.row/sum(prob.row)
prob.col = prob.col/sum(prob.col)

prob = array(prob.row, c(5,1))%x% array(prob.col, c(1,5))
# for(i in 1:5)
#   for(j in 1:5)
#     prob[i,j] =prob.row[i] + prob.col[j]
# 
# prob= prob/sum(prob)

x = array( rmultinom(1, n, prob), c(5,5))
chisq.test(x)

comb.all = combn(5,2)
pvalue = array(0, c(10,10))
xstat = array(0, c(10,10))
for( i in 1:10)
  for(j in 1:10)
  {
    pvalue[i,j]= chisq.test( array( c(x[comb.all[,i], comb.all[,j]],
             x[comb.all[,i], -comb.all[,j]],
             x[-comb.all[,i], comb.all[,j]],
             x[-comb.all[,i], -comb.all[,j]]
    ), c(2,2)) )$p.value
    xstat[i,j]= chisq.test( array( c(x[comb.all[,i], comb.all[,j]],
                                      x[comb.all[,i], -comb.all[,j]],
                                      x[-comb.all[,i], comb.all[,j]],
                                      x[-comb.all[,i], -comb.all[,j]]
    ), c(2,2)) )$stat
  }

chisq.test(x)
1-pchisq( sum(-2*log(pvalue)), 100 )