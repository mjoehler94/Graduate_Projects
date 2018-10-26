#Matt Oehler
#Stat 624 Project 2
#Application
#


#EDA for hepatitis data
dat = read.csv("hep_clean.csv",header=T)[,-1]
head(dat)
dim(dat)

#leave out categorical data
hepdat <- dat[,c('Age','Bilirubin','AlkPhosphate','Sgot','AlbuMin','ProTime')]
apply(hepdat,2,hist)#make histograms of every variable


##THROW AWAY METHOD
rem.vals <- hepdat[complete.cases(hepdat),]
dim(rem.vals)

ta.mean.est <- apply(rem.vals,2,mean)
ta.mean.est
ta.var.est <- var(rem.vals)
ta.var.est

## EM ALGORITHM
library(norm) #for em.norm
# data(mdata)
# s <- prelim.norm(mdata) #do preliminary manipulations
# thetahat <- em.norm(s) #compute mle
# getparam.norm(s,thetahat,corr=TRUE)$r #look at estimated correlations
# getparam.norm(s,thetahat,corr=F) #get means and covariance

s <- prelim.norm(as.matrix(hepdat))#do preliminary manipulations
thetahat <- em.norm(s) #compute mle using EM Algorithm
getparam.norm(s,thetahat,corr=T)$r #look at estimated correlations
getparam.norm(s,thetahat,corr=F) #get means and covariance estimates

em.mean.est <- getparam.norm(s,thetahat,corr=F)$mu
em.mean.est
em.var.est <- getparam.norm(s,thetahat,corr=F)$sigma
em.var.est

#mycoded up EM Algorithm
source('sourcefunctions.R')
my.em.est <- EMalg(hepdat,tol=0.01)

### Conditional Sampling
cs.est <- conditional_sampling(as.matrix(hepdat),n.draws=1000,n.burn=100)
cs.mean.est <- cs.est$MeanEst
cs.mean.est
cs.var.est <- cs.est$CovarianceEst
cs.var.est

#xtable code
all.means <- matrix(c(ta.mean.est,em.mean.est,cs.est$MeanEst),nrow=6,ncol=3)
rownames(all.means) <- c('Age','Bilirubin','AlkPhosphate','Sgot','AlbuMin','ProTime')
colnames(all.means) <- c('Throw Away', 'EM Algorithm', 'Conditional Sampling')
all.means
print(xtable(round(all.means,2)))


##MAKE PLOTS ###
pdf('goodnessoffit.pdf')
#plot the fit of the estimate on a marginal
par(mfrow=c(2,3))
#1 Age
hist(hepdat$Age,col='grey',prob=T,breaks = 20,main='',xlab='Age')
lines(density(na.exclude(hepdat$Age)),col='black',lwd=2)
curve(dnorm(x,ta.mean.est[1],sqrt(ta.var.est[1,1])),lwd=3,lty=1,col = 'red',add=T)#throw away
curve(dnorm(x,em.mean.est[1],sqrt(em.var.est[1,1])),lwd=3,lty=1,col = 'blue',add=T)#em alg
curve(dnorm(x,cs.mean.est[1],sqrt(cs.var.est[1,1])),lwd=3,lty=2,col = 'green',add=T)#cond samp
# legend('topright',legend=c('Hep. Data','Throw Away','EM Alg','Cond. Samp.'),
#        # col=c('black','red','blue','green'),
#        fill = c("black", "red", "blue","green")
# )

#2 Bilirubin
hist(hepdat$Bilirubin,col='grey',prob=T,breaks = 20,main='',xlab='Bilirubin')
lines(density(na.exclude(hepdat$Bilirubin)),col='black',lwd=2)
curve(dnorm(x,ta.mean.est[2],sqrt(ta.var.est[2,2])),lwd=3,lty=1,col = 'red',add=T)#throw away
curve(dnorm(x,em.mean.est[2],sqrt(em.var.est[2,2])),lwd=3,lty=1,col = 'blue',add=T)#em alg
curve(dnorm(x,cs.mean.est[2],sqrt(cs.var.est[2,2])),lwd=3,lty=2,col = 'green',add=T)#cond samp
legend('topright',legend=c('Hep. Data','Throw Away','EM Alg','Cond. Samp.'),
       # col=c('black','red','blue','green'),
       fill = c("black", "red", "blue","green")
)

#3AlkPhosphate
hist(hepdat$AlkPhosphate,col='grey',prob=T,breaks = 20,main='',xlab='AlkPhospate')
lines(density(na.exclude(hepdat$AlkPhosphate)),col='black',lwd=2)
curve(dnorm(x,ta.mean.est[3],sqrt(ta.var.est[3,3])),lwd=3,lty=1,col = 'red',add=T)#throw away
curve(dnorm(x,em.mean.est[3],sqrt(em.var.est[3,3])),lwd=3,lty=1,col = 'blue',add=T)#em alg
curve(dnorm(x,cs.mean.est[3],sqrt(cs.var.est[3,3])),lwd=3,lty=2,col = 'green',add=T)#cond samp
# legend('topright',legend=c('Hep. Data','Throw Away','EM Alg','Cond. Samp.'),
#        # col=c('black','red','blue','green'),
#        fill = c("black", "red", "blue","green")
# )
#4Sgot
hist(hepdat$Sgot,col='grey',prob=T,breaks = 20,main='',xlab='Sgot')
lines(density(na.exclude(hepdat$Sgot)),col='black',lwd=2)
curve(dnorm(x,ta.mean.est[4],sqrt(ta.var.est[4,4])),lwd=3,lty=1,col = 'red',add=T)#throw away
curve(dnorm(x,em.mean.est[4],sqrt(em.var.est[4,4])),lwd=3,lty=1,col = 'blue',add=T)#em alg
curve(dnorm(x,cs.mean.est[4],sqrt(cs.var.est[4,4])),lwd=3,lty=2,col = 'green',add=T)#cond samp
# legend('topright',legend=c('Hep. Data','Throw Away','EM Alg','Cond. Samp.'),
#        # col=c('black','red','blue','green'),
#        fill = c("black", "red", "blue","green")
# )
#5AlbuMin
hist(hepdat$AlbuMin,col='grey',prob=T,breaks = 20,main='',xlab='AlbuMin')
lines(density(na.exclude(hepdat$AlbuMin)),col='black',lwd=2)
curve(dnorm(x,ta.mean.est[5],sqrt(ta.var.est[5,5])),lwd=3,lty=1,col = 'red',add=T)#throw away
curve(dnorm(x,em.mean.est[5],sqrt(em.var.est[5,5])),lwd=3,lty=1,col = 'blue',add=T)#em alg
curve(dnorm(x,cs.mean.est[5],sqrt(cs.var.est[5,5])),lwd=3,lty=2,col = 'green',add=T)#cond samp
# legend('topright',legend=c('Hep. Data','Throw Away','EM Alg','Cond. Samp.'),
#        # col=c('black','red','blue','green'),
#        fill = c("black", "red", "blue","green")
# )

#6 Protime 
hist(hepdat$ProTime,col='grey',prob=T,breaks = 20,main='',xlab='ProTime')
lines(density(na.exclude(hepdat$ProTime)),col='black',lwd=2)
curve(dnorm(x,ta.mean.est[6],sqrt(ta.var.est[6,6])),lwd=3,lty=1,col = 'red',add=T)#throw away
curve(dnorm(x,em.mean.est[6],sqrt(em.var.est[6,6])),lwd=3,lty=1,col = 'blue',add=T)#em alg
curve(dnorm(x,cs.mean.est[6],sqrt(cs.var.est[6,6])),lwd=3,lty=2,col = 'green',add=T)#cond samp
# legend('topright',legend=c('Hep. Data','Throw Away','EM Alg','Cond. Samp.'),
#        # col=c('black','red','blue','green'),
#        fill = c("black", "red", "blue","green")
# )

dev.off()




