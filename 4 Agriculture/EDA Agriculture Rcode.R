
### Agriculture Data EDA

#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 4 Agriculture")
agdat <- read.table('AgricultureWater.txt',header = T)
head(agdat)

plot(agdat)
plot(agdat$cwsi,agdat$swc,main='Crop Data',
     xlab='CWSI',ylab='SWC')


#
