## the number of births per month in New York city, from January 1946 to December 1959 (originally collected by Newton).

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")

month = c(1:length(births) )

births=births[30:168]
month=month[30:168]

birth.lm = lm( births ~ month )

plot(month, births,'l')
abline(birth.lm, col='red')

plot(month, birth.lm$residuals)