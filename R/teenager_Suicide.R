


########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################
########################################################################################################################################################

# ------------------------------------------------------------------------
# Teenage Suicide Example: Hypothesis Testing
# ------------------------------------------------------------------------

suicide <-  c(1867,1789,1944,2094,2097,1981,1887,2024,1928,2039,1978,1859)
days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
month <- 1:12
names(month) <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
dat <- data.frame(month,days,suicide,suicide/days)
names(dat) <- c("month","days","suicide","rate per day")
dat

#postscript(file=".figure/suicide.eps",horizontal=FALSE)
par(mfrow = c(1,2),oma = c(.5,.5,.5,.5))
plot(1:12,suicide,type = "b",ylab = "Number of Teenage Suicide",
     xlab = "Month", xaxt = "n")
axis(1, at = 1:12, labels = names(month))
plot(1:12, suicide/days, type="b", ylab="Rate of Tennage suicides",
     xlab = "Month", xaxt="n")
axis(1, at=1:12, labels = names(month))
#dev.off()

eio <- days/365*sum(suicide)
two.log.lambda <- 2*sum(suicide*(log(suicide)-log(eio)))
cat("two.log.lambda is", two.log.lambda,"\n")

cutoff <- qchisq(.95,11)
cat("Cutoff is", cutoff, "\n")

p.val <- 1 - pchisq(two.log.lambda,11)
cat("pvalue is", p.val, "\n")

