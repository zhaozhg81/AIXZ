NBA <- read.table("data/NBA_Payer_Salary.csv",header=T, sep=",")

NBA$X2019 <- round(NBA$X2019/1000000,digits=2)
NBA$X2020 <- round(NBA$X2020/1000000,digits=2)
NBA$X2021 <- round(NBA$X2021/1000000,digits=2)

XEast <- NBA$X2019[ NBA$Conference=="east"]
XWest <- NBA$X2019[ NBA$Conference=="west"]
