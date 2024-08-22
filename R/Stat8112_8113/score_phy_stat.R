
dat = read.table("./data/score_phy_stat.dat",head=T)
plot(dat$Phys, dat$Stat)

pc = princomp(~Stat+Phys,dat)

pc$loading
