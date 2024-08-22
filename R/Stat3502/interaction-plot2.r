# run interaction_depression.r first


par (mfrow = c (1, 1))

plot(x1[-c(ind2,ind3)],y[-c(ind2,ind3)],xlab="age",ylab="effectiveness",
     xlim=c(19,70),ylim=c(25,75))
points(x1[ind2],y[ind2],pch=3)
points(x1[ind3],y[ind3],pch=6)

legend("bottomright", inset=.05, pch=c(1,3,6),
       c("A","B","Placebo"))

plot(x1[-c(ind2,ind3)],y[-c(ind2,ind3)],xlab="age",ylab="effectiveness",
     xlim=c(19,70),ylim=c(25,75))
points(x1[ind2],y[ind2],pch=3)
points(x1[ind3],y[ind3],pch=6)

abline(a=beta_hat[1],b=beta_hat[2],lty=1)
abline(a=beta_hat[1]+beta_hat[3],b=beta_hat[2]+beta_hat[5],lty=2)
abline(a=beta_hat[1]+beta_hat[4],b=beta_hat[2]+beta_hat[6],lty=3)


legend("bottomright", inset=.05, pch=c(1,3,6),
       c("A","B","Placebo"),lty=c(1,2,3))

