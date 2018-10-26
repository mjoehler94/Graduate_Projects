# Ag Report R code
# Matt Oehler
# Stat 536

## NOTES ##
# We started out wanting to use splines, but splines showed us that zero knots
# is best, and the cubic term for the default cubic spline wasn't significant
# so we decided that just a quadratic polynomial fit would be best
# We will verify this with cross validation


## Read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 4 Agriculture/Ag Report")
agdat <- read.table('AgricultureWater.txt',header = T)
head(agdat)

plot(agdat$cwsi,agdat$swc,main='Crop Data',
     xlab='CWSI',ylab='SWC')


# fit a polynomial model (cubic and quadratic)
fit.poly <- lm(swc~poly(cwsi,3),data=agdat)
summary(fit.poly)
coef(summary(fit.poly))

round(xtable(summary(fit.poly)),2)

fit.poly2 <- lm(swc~poly(cwsi,2,raw=T),data=agdat)
summary(fit.poly2)
round(xtable::xtable(summary(fit.poly2)),2)

# graph model to see fit

cwsi.lims <- range(agdat$cwsi)
cwsi.grid <- seq(from=cwsi.lims[1],to=cwsi.lims[2],length = 50)
cwsi.grid
polypreds <- predict(fit.poly,newdata = list(cwsi=cwsi.grid),se=T)
length(polypreds$fit)
se.bands <- cbind(polypreds$fit+2*polypreds$se.fit,polypreds$fit-2*polypreds$se.fit)

#for cubic polynomial
pdf('cubicfit.pdf')
plot(agdat$cwsi,agdat$swc,main='Cubic Fit',xlab='CWSI',ylab='SWC')
lines(cwsi.grid,polypreds$fit,lwd=2,col='red')
matlines(cwsi.grid,se.bands,lwd=1,col='blue',lty=1)

#add prediction bands
pred.var <-sqrt( polypreds$se.fit^2 + var(fit.poly$residuals) )
pred.bands <- cbind(polypreds$fit+2*pred.var,polypreds$fit-2*pred.var)
matlines(cwsi.grid,pred.bands,lwd=1,col='darkgreen',lty=1)
dev.off()

#for quadratic polynomial
pdf('quadraticfit.pdf')
plot(agdat$cwsi,agdat$swc,main='Quadratic Fit',xlab='CWSI',ylab='SWC')
lines(cwsi.grid,polypreds$fit,lwd=2,col='red')
polypreds2 <- predict(fit.poly2,newdata = list(cwsi=cwsi.grid),se=T)
se.bands2 <- cbind(polypreds2$fit+2*polypreds2$se.fit,polypreds2$fit-2*polypreds2$se.fit)
pred.var2 <-sqrt( polypreds2$se.fit^2 + var(fit.poly2$residuals) )
pred.bands2 <- cbind(polypreds2$fit+2*pred.var2,polypreds2$fit-2*pred.var2)
matlines(cwsi.grid,se.bands2,lwd=1,col='blue',lty=1)
matlines(cwsi.grid,pred.bands2,lwd=1,col='darkgreen',lty=1)
dev.off()

##verify assumptions
pdf('residplot.pdf')
plot(fit.poly2$fitted.values,fit.poly$residuals,pch=19,main='Residual Plot',
     ylab='',xlab='')
abline(h=0, col='red')
dev.off()

pdf('residhist.pdf')
hist(fit.poly2$residuals,prob=T,main='Histogram of Residuals',ylab='',xlab='')
curve(dnorm(x,0,sd(fit.poly2$residuals)),col='red',lwd = 2,add=T)
dev.off()

#### Performance evaluation of final model
#Asssess both quadratic and cubic
mod.degree <- 2
iters <- 5000
mse <- rep(NA,iters)
bias <- rep(NA,iters)
coverage <- rep(NA,iters)
rpmse <- rep(NA,iters)
N <- nrow(agdat)
for(i in 1:iters){
  #make test and train data set
  test.ind <- sample(1:nrow(agdat),ceiling(N*.15))
  test.dat <- agdat[test.ind,]
  train.dat <- agdat[-test.ind,]
  #train the model
  model <- lm(swc~poly(cwsi,mod.degree),data=train.dat)
  #assess results
  pred.vals <- predict(model,newdata = test.dat,interval = 'prediction',level=0.95)
  coverage[i] <- mean(test.dat$swc > pred.vals[,2] & test.dat$swc < pred.vals[,3])
            # mean(test.data2$swc > test.preds2[,2] & test.data2$swc < test.preds2[,3])
  piw <- mean(pred.vals[,3] - pred.vals[,2])
  bias[i] <- mean(test.dat$swc - pred.vals[,1])
  mse[i] <- mean((test.dat$swc - pred.vals[,1])^2)
  rpmse[i] <- sqrt(mean((test.dat$swc - pred.vals[,1])^2))
}
mean(bias)
mean(mse)
mean(rpmse)
mean(coverage)
mean(piw)

name.vec <- c('Bias','RPMSE','Coverage','PIW','$R^{2}$')
Cubic <- c(mean(bias)
           ,mean(rpmse)
           ,mean(coverage)
           ,mean(piw))
Quadratic <- c(mean(bias)
               ,mean(rpmse)
               ,mean(coverage)
               ,mean(piw))
xtable::xtable(cbind(' '=name.vec,Cubic=round(c(Cubic,.9689),4),
                     Quadratic=round(c(Quadratic,0.9689),4)))

xtable::xtable(cbind(' '=name.vec,Quadratic=round(c(Quadratic,0.9689),4)))


##############side notes
coef(fit.poly)
coef(fit.spline3)
#do splines not orthogonalize




#using splines
library(splines)
fit.spline <- lm(swc~bs(cwsi,knots=c(.4,.7)),data = agdat)
cwsi.grid <- seq(from=cwsi.lims[1],to=cwsi.lims[2],length = 50)
pred.spline <- predict(fit.spline,newdata = list(cwsi=cwsi.grid),se=T)
lines(cwsi.grid,pred.spline$fit,lwd=2,col='cornflower blue')
lines(cwsi.grid,pred.spline$fit + 2*pred.spline$se.fit,lty='dashed')
lines(cwsi.grid,pred.spline$fit - 2*pred.spline$se.fit,lty='dashed')

fit.spline3 <- lm(swc~bs(cwsi,df=3),data = agdat)
pred.spline3 <- predict(fit.spline3,newdata = list(cwsi=cwsi.grid),se=T)
plot(agdat$cwsi,agdat$swc,main='Crop Data',
     xlab='CWSI',ylab='SWC')
lines(cwsi.grid,pred.spline3$fit,lwd=2,col='cornflower blue')
lines(cwsi.grid,pred.spline3$fit + 2*pred.spline$se.fit,lty='dashed')
lines(cwsi.grid,pred.spline3$fit - 2*pred.spline$se.fit,lty='dashed')
attr(bs(agdat$cwsi,df=3),'knots')



dim(bs(agdat$cwsi,knots=c(.4,.7)))
dim(bs(agdat$cwsi,df=5))
attr(bs(agdat$cwsi,df=6),'knots')
abline(v=attr(bs(agdat$cwsi,df=6),'knots'))#auto chosen knots
abline(v=c(.4,.7),col='firebrick')

#natural spline
fit.spline2 <- lm(swc~bs(cwsi,df=4),data = agdat)
pred2 <- predict(fit.spline2,newdata = list(cwsi=cwsi.grid),se=T)
lines(cwsi.grid,pred2$fit,lwd=2,col='aquamarine')

#BOOK Data
agelims <- range(age)
age.grid <- seq(from=agelims[1],agelims[2])
age.grid
