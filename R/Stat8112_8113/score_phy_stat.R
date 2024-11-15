set.seed(2)
English = rnorm(100, 75, 2)


dat = read.table("./data/score_phy_stat.dat",head=T)
dat = read.table("./data/3502/score_phy_stat_english.dat",head=T)

plot( dat$Phys, dat$English, xlim=c(0,100), ylim=c(0,100))


plot(dat$Phys, dat$Stat, xlim=c(0,100), ylim=c(0,100))

pc = princomp(~Stat+Phys,dat)

pc$loading
