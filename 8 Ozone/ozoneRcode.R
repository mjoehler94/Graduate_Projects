#######################
### 536 Exam 1 Code ###
### Matt Oehler     ###
### Dr. Heaton      ###
#######################

##questions
# what are the units of ozone
# what is the goal
# 

## read in data
# station
setwd("~/Matt BYU/10 Winter 2018/Stat 536/Exam 1 Ozone")
station <- read.csv('Ozone.csv',header=T)
dim(station)
head(station)

#cmaq
cmaq <- read.csv('CMAQ.csv',header=T)
dim(cmaq)
head(cmaq)

#pred locations
predlocs <- read.csv('PredLocs.csv',header=T)
head(predlocs)
dim(predlocs)
# predlocs$fill <- 1

#make a plot of the data
library(maps)
library(LatticeKrig)

# cmaq
# pdf('cmaq.pdf')
quilt.plot(cmaq$Longitude,cmaq$Latitude,cmaq$CMAQ_O3,
           main='CMAQ')
map('state',add=T)
# dev.off()

#station
# pdf('station.pdf')
quilt.plot(station$Longitude,station$Latitude,station$Ozone.8.hr.max.,
           main='Station')
map('state',add=T)
# dev.off()
#predlocs
pdf('predlocs.pdf')
# quilt.plot(predlocs$Longitude,predlocs$Latitude,predlocs$fill)
plot(predlocs$Longitude,predlocs$Latitude,pch=20,cex=.5,
     main='Prediction Locations',xlab='Longitude',ylab='Latitude')
map('state',add=T)
dev.off()




# I can merge the cmaq points with station data set to give me a 
# data set of 800 with cmaq, station, longitude, and latitude


distp1p2 <- function(p1,p2) {
  dst <- sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
  return(dst)
}

dist2 <- function(y) min(apply(set2, 1, function(x) min(distp1p2(x,y))))
dist3 <- function(y) which.min(apply(set2, 1, function(x) min(distp1p2(x,y))))
set1 <- tests[,2:3]
set2 <- testc[,1:2]
foo <- apply(set1, 1, dist2)
#takes about 6 minutes
system.time(
foo2 <- apply(set1, 1, dist3)
)
#write data to a csv once it is merged properly
vec <- cmaq[foo2,4]
a <- cbind(tests[,-1],cmaq = vec)
names(a) <- c('long','lat','station','cmaq')
head(a)
merged <- a
# write.csv(a,'merged.csv')

# get cmaq for pred regions
#run time is approx: 
set3 <- predlocs[,1:2]
system.time(
  foo3 <- apply(set3, 1, dist3)
)
vec2 <- cmaq[foo3,4]
b <- cbind(predlocs,cmaq = vec2)
# write.csv(b,'predmerged.csv')



##fit a model with a nugget and exponential structure
#I want to use exponential covariance structure because it will be useful in 
#accounting for the non-constant distance between points.

# spat.spexp <- gls(yield ~ 1,data=Spat,correlation = corExp(form= ~row+col))
# summary(spat.spexp)

# spat.spexpv <- gls(yield ~ 1,data=Spat,correlation = corExp(value=.1,form= ~row+col|grp))
# summary(spat.spexpv)

## GLS of Exponential Errors w/nugget
# gls(y~X,correlation=corExp(form=~time,nugget=TRUE),data=mydata,method="ML")
clean.dat <- read.csv('merged.csv',header=T)[,-1]
head(clean.dat)

clean.pred <- read.csv('predmerged.csv',header = T)[,-1]
head(clean.pred)

#plot of cmaq matched with stations
# plot(cmaq[foo2,1],cmaq[foo2,2])
quilt.plot(clean.dat$long,clean.dat$lat,clean.dat$cmaq)
map('state',add=T,col='black')
points(station[,2],station[,3],pch=0,col='red')

#plot of cmaq matched with predlocs
plot(predlocs$Longitude,predlocs$Latitude,pch=20,cex=.5)
map('state',add=T,col='blue')
points(cmaq[foo3,1],cmaq[foo3,2],pch=20,col='red',cex=.5)

library(nlme)
# mod1 <- Gls(station ~ cmaq,correlation = corExp(form= ~ long + lat, nugget=T),
#             data = clean.dat,method='ML',x=T)
# summary(mod1)
# intervals(mod1)
mod2 <- gls(station ~ cmaq,correlation = corExp(form= ~ long + lat, nugget=T),
            data = clean.dat,method='ML')
summary(mod2)
intervals(mod2)

# prediction

nug <- 0.1738894
phi <- 5.4519892
N <- nrow(clean.dat)
K <- nrow(predlocs) # Number of locations that I want to predict
r <- diag(K+N)
# r <- -exp(-as.matrix(dist(1:(K+N)))/phi)
R <- (1-nug)*r+nug*diag(N+K) ##Exp correlation with nugget
dim(R)

sig2 <- mod2$sigma^2
sig2
Xstar <- cbind(1,clean.pred$cmaq) #xmatrix of predlocs and matched cmaq values
bhat <- t(t(mod2$coefficients))
X <- cbind(1,clean.dat$cmaq)
Y <- clean.dat$station

pred.mn <- Xstar%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
  (Y-X%*%bhat) #conditional mean of MVN
pred.mn

pred.var <- sig2*(R[N+(1:K),N+(1:K)]-R[N+(1:K),
                                       (1:N)]%*%solve(R[(1:N),(1:N)])%*%R[(1:N),N+(1:K)]) # conditional variance of MVN
#compare plots
#original
par(mfrow=c(2,1))
quilt.plot(station$Longitude,station$Latitude,station$Ozone.8.hr.max.,
           main='original')
map('state',add=T)
#predicted mean
# pdf('predquilt.pdf')
quilt.plot(clean.pred$Longitude,clean.pred$Latitude,pred.mn,
           main='Predicted Values')
map('state',add=T)
# dev.off()
##############################
#model with no intercept
mod3 <- gls(station ~ poly(cmaq,4,raw=TRUE), correlation = corExp(form= ~ long + lat, nugget=T),
            data = clean.dat,method='ML')
summary(mod3)
intervals(mod3)

nug3 <- 0.1855989
phi <- 5.9058012
N <- nrow(clean.dat)
K <- nrow(predlocs) # Number of locations that I want to predict
# r <- diag(K+N)
r <- -exp(-as.matrix(dist(1:(K+N)))/phi)

R <- (1-nug3)*r+nug3*diag(N+K) ##Exp correlation with nugget
dim(R)

sig2.3 <- mod3$sigma^2
sig2.3
Xstar.3 <- cbind(1,clean.pred$cmaq,clean.pred$cmaq^2,clean.pred$cmaq^3,clean.pred$cmaq^4) #xmatrix of predlocs and matched cmaq values

bhat.3 <- t(t(mod3$coefficients))
X.3<- cbind(1,clean.dat$cmaq,clean.dat$cmaq^2,clean.dat$cmaq^3,clean.dat$cmaq^4)
Y <- clean.dat$station

pred.mn3 <- Xstar.3%*%bhat.3 + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
  (Y-X.3%*%bhat.3) #conditional mean of MVN
pred.mn3

pred.var3 <- sig2.3*(R[N+(1:K),N+(1:K)]-R[N+(1:K),
                                       (1:N)]%*%solve(R[(1:N),(1:N)])%*%R[(1:N),N+(1:K)]) # conditional variance of MVN
pred.var3

#predicted mean
quilt.plot(clean.pred$Longitude,clean.pred$Latitude,pred.mn3,
           main='predicted values')
map('state',add=T)


# ### De-correlate model to check assumptions
# my.X <- model.matrix(fitar.ps,data=solar)
my.X <- model.matrix(mod2, data=clean.dat)
dim(my.X)
# 
# #de-correlate model to be able to check assumptions
# # L.inv * Y ~ N(L.inv * X * beta, L.inv * L * t(L) * t(L.inv))
# #make R
# n <- n.sn
# k <- n.sy 
# R <- diag(k+n)
# R <- sigma2*phi^(abs(row(R) - col(R)))
N <- nrow(clean.dat)
K <- 0#nrow(predlocs) # Number of locations that I want to predict

# r <- diag(K+N)
phi <- 5.4519892
r <- -exp(-as.matrix(dist(1:(K+N)))/phi)

R <- (1-nug)*r+nug*diag(N+K) ##Exp correlation with nugget
dim(R)
# 
L<- t(chol(R))
# Ystar <- solve(L) %*% solar$PowerBill
Ystar <- solve(L) %*% clean.dat$station
# 
XX.star <- solve(L) %*%  my.X
decorr.mod <- lm(Ystar ~ XX.star) #fit de-correlated model
summary(decorr.mod)
# #check assumptions
#linearity 
# pdf('linplot.pdf')
plot(XX.star[,2],Ystar,main='Linearity Assumption')
# dev.off()
# #residuals
# pdf('residplot.pdf')
plot(decorr.mod$fitted.values,decorr.mod$residuals,
     main='Fitted Values vs. Residuals',xlab='Fitted Values',ylab='Residuals')
abline(h=0,col='red')
# dev.off()
# pdf('residhist.pdf')
hist(decorr.mod$residuals,prob=T,
     main='Histogram of Residuals',
     xlab='Residuals')
curve(dnorm(x,0,sd(decorr.mod$residuals)),col='red',add=T)
# dev.off()


# cross validation of model ########################
iters <- 200
mse <- rep(NA,iters)
bias <- rep(NA,iters)
coverage <- rep(NA,iters)
rpmse <- rep(NA,iters)
n <- nrow(clean.dat)
for(i in 1:iters){
  #make test and train data set
  test.ind <- sample(1:n,ceiling(N*.15))
  test.dat <- clean.dat[test.ind,]
  train.dat <- clean.dat[-test.ind,]
  #train the model
  # model <- lm(swc~poly(cwsi,mod.degree),data=train.dat)
  model <- gls(station ~ cmaq,correlation = corExp(form= ~ long + lat, nugget=T),
               data = clean.dat,method='ML')
  
  #assess results
  # pred.vals <- predict.(model,newdata = test.dat,interval = 'prediction',level=0.95)
  # coverage[i] <- mean(test.dat$swc > pred.vals[,2] & test.dat$swc < pred.vals[,3])
  
  nug <- 0.1738894
  N <- nrow(train.dat)
  K <- nrow(test.dat) # Number of locations that I want to predict
  r <- diag(K+N)
  R <- (1-nug)*r+nug*diag(N+K) ##Exp correlation with nugget
  sig2 <- model$sigma^2
  Xstar <- cbind(1,test.dat$cmaq) #xmatrix of predlocs and matched cmaq values
  bhat <- t(t(model$coefficients))
  X <- model.matrix(model,data=train.dat)
  Y <- train.dat$station
  pred.mn <- Xstar%*%bhat + R[N+(1:K), (1:N)]%*%solve(R[(1:N),(1:N)])%*%
    (Y-X%*%bhat) #conditional mean of MVN
  
  bias[i] <- mean(pred.mn - test.dat$station)
  mse[i] <- mean((test.dat$station - pred.mn)^2)
  rpmse[i] <- sqrt(mean((test.dat$station - pred.mn)^2))
  
  # mean(test.data2$swc > test.preds2[,2] & test.data2$swc < test.preds2[,3])
  # piw <- mean(pred.vals[,3] - pred.vals[,2])
}
mean(bias)
mean(mse)
mean(rpmse)
mean(coverage)
mean(piw)

bias.200 <- mean(bias)
mse.200 <- mean(mse)
rpmse.200 <- mean(rpmse)

cv.tab <- rbind(Bias=bias.200,MSE = mse.200,RPMSE = rpmse.200)
xtable(cv.tab)
###make tables
library(xtable)

modcoefs <- t(t(coef(mod2)))
modints <- intervals(mod2)$coef
xtable(modints)






######################################################
#scratch code
hist(clean.dat$station)
hist(clean.dat$cmaq)


ab <- c(1:10) + rnorm(10)
cd <- c(1:10)

foomod <- lm(ab~cd,x=T)
fmod <- lm(ab~-1 + cd,x=T)

plot(cd,ab,xlim=c(0,11),ylim=c(0,11))
abline(foomod,col='blue')
abline(fmod,col='red')
abline(h=0,lty=2)
abline(v=0,lty=2)

m2 <- gls(station ~ cmaq,data=clean.dat)

vario2 <- Variogram(m2, form = ~long + lat, resType = "pearson")
plot(vario2, smooth = TRUE)#, ylim = c(0, 1.2))





