# Chapter 5, Section 5.8: Multiple Testing
# Real A/B-testing replication data; no q-value method.
d <- read.csv("./data/fdr-data.csv")
p <- d$p_value
m <- length(p)

cat("tests =",m," experiments =",length(unique(d$experiment_id)),"\n")
cat("raw p<=.05 =",sum(p<=.05),"\n")

hist(p,breaks=seq(0,1,.05),freq=FALSE,
     main="P-values from Real A/B Experiments",xlab="p-value")
abline(h=1,lty=2)

p.bh <- p.adjust(p,"BH")
p.bonf <- p.adjust(p,"bonferroni")
print(data.frame(
 method=c("Unadjusted","BH q=.05","Bonferroni FWER=.05"),
 discoveries=c(sum(p<=.05),sum(p.bh<=.05),sum(p.bonf<=.05))))

# Direct BH calculation
ps <- sort(p); q <- .05
k <- max(which(ps <= (1:m)/m*q))
cat("BH k =",k," cutoff =",ps[k],"\n")

# In the real data, true-null status is NOT observed.
# Simulation below makes truth known.
sim <- function(m=5000,pi0=.70,effect=2.5,q=.05){
  null <- rbinom(m,1,pi0)==1
  theta <- ifelse(null,0,effect)
  z <- rnorm(m,theta,1)
  pv <- 2*pnorm(-abs(z))
  rej <- p.adjust(pv,"BH")<=q
  R <- sum(rej); V <- sum(rej & null)
  c(R=R,V=V,FDP=ifelse(R==0,0,V/R))
}
set.seed(2026)
sim()
mean(replicate(1000,sim()["FDP"]))
