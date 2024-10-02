alcoholarm=read.table("data/3502/alcoholarm.txt",header=T)
head(alcoholarm)

alcohol.lm = lm(strength~alcohol, data=alcoholarm)

plot(alcoholarm$alcohol, alcoholarm$strength)
abline( alcohol.lm, col='red')

plot( alcohol.lm$fitted.values, alcohol.lm$residuals)
abline(0,0, col='red')

plot( alcoholarm$alcohol, alcohol.lm$residuals)
abline(0,0, col='red')
