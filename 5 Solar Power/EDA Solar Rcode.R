# EDA R CODE FOR SOLAR SAVINGS DATA

#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 5 Solar Power")
solar <- read.csv('SolarSavings.csv',header=T)
solar$obs <- 1:nrow(solar)
dim(solar)
solar

boxplot(PowerBill~Solar,data=solar,main = 'PowerBill for Solar/Non-solar')

#make time series plot
plot(solar$PowerBill[solar$Solar=='N'],type='l',lwd=1,
     xaxt = 'n',xlab='',ylab='Power Bill',main='Payment History')
axis(1, at = seq(1, 51, by = 5),labels=solar$Date[seq(1, 51, by = 5)], las=2)
colors <- c('blue','red')
my.pch <- c(1,16)
points(1:nrow(solar),solar$PowerBill,col=colors[solar$Solar],pch=my.pch[solar$Solar])
abline(v=which(solar$Solar=='Y')[1],col='red',lty=)

#make a better time series plot with separation
plot(solar$PowerBill[solar$Solar=='N'],xlim=c(0,52),ylim=c(0,240),type='l',lwd=1,
     xaxt = 'n',xlab='',ylab='Power Bill',main='Payment History',lty=2)
lines((1:nrow(solar))[solar$Solar=='Y'],solar$PowerBill[solar$Solar=='Y'],
      lty=1,col='blue')
axis(1, at = seq(1, 51, by = 5),labels=solar$Date[seq(1, 51, by = 5)], las=2)
legend("topright",col=c("black","blue"),legend=c("Regular", "Solar"),
       lty=c(2,1),lwd=c(1))

# fit.lm <- lm(PowerBill~Solar,data = solar)
# summary(fit.lm)
# plot(fit.lm)

#ar model for peak season
# effect for peak season
fitar.ps <- gls(PowerBill ~ -1 + as.factor(season):Solar, correlation = corARMA(form=~rtime|Solar,p=1,q=0),data=solar,method="ML")
summary(fitar.ps)


#make time series plot with prediction line
plot(solar$PowerBill[solar$Solar=='N'],xlim=c(0,52),ylim=c(0,240),type='l',lwd=1,
     xaxt = 'n',xlab='',ylab='Power Bill',main='AR(1) Model Fit',lty=2)
lines((1:nrow(solar))[solar$Solar=='Y'],solar$PowerBill[solar$Solar=='Y'],lty=2)
axis(1, at = seq(1, 51, by = 5),labels=solar$Date[seq(1, 51, by = 5)], las=2)
colors <- c('blue','red')
lines(predict(fitar.ps)[solar$Solar=='N'],type="l",col='blue',lwd = 2,lty=1)
lines((1:nrow(solar))[solar$Solar=='Y'],predict(fitar.ps)[solar$Solar=='Y'],
      type="l",col='blue',lwd = 2,lty=1)
legend("topright",col=c("black","blue"),legend=c("Data", "Model"),
       lty=c(2,1),lwd=c(1,2))

#find the value of R^2 squared for the model
ss.res <- sum( (solar$PowerBill - predict(fitar.ps))^2 )
ss.total <- sum( (solar$PowerBill - mean(solar$PowerBill))^2 )

Rsquared <- 1-(ss.res/ss.total)
Rsquared

### code from slides
## GLS with AR(1) Errors
# ar1.gls <- gls(y~X,correlation=corAR1(form=~time),data=mydata,method="ML")
# summary(ar1.gls)
# intervals(ar1.gls)
# ## GLS with MA(1) Errors
# ma1.gls <-
#   gls(y~X,correlation=corARMA(form=~time,p=0,q=1),data=mydata,method="ML")
# ## GLS of Exponential Errors w/nugget
# exp.gls <-
#   gls(y~X,correlation=corExp(form=~time,nugget=TRUE),data=mydata,method="ML")

# Seasonality Parameter:
# March 20 at 11:02 UTC	Vernal (Spring) equinox
# June 21 at 05:04 UTC	Summer solstice
# September 22 at 20:44 UTC	Autumnal equinox
# December 21 at 17:11 UTC	Winter solstice

class(solar$Date)
head(solar$Date)
my_date <- as.Date(solar$Date)
my_date[1] > my_date[2] 

my_date <- strsplit(as.character(solar$Date),'/')
my_date <- sapply(my_date,'[',1)
my_season <- as.factor(abs(as.numeric(my_date) %% 4 - 4))


clean_data <- cbind(solar,my_date)
head(clean_data)



library(nlme)
## GLS with AR(1) Errors
# ar1.gls <- gls(y~X,correlation=corAR1(form=~time),data=mydata,method="ML")
# summary(ar1.gls)

ar1 <- gls(PowerBill ~ Solar, correlation = corAR1(form =~obs),
           method='ML', data = solar)
summary(ar1)



####################################
solar <- read.csv("SolarSavings.csv",as.is=TRUE)
solar$rtime <- c(1:sum(solar$Solar == "N") - 1, 1:sum(solar$Solar == "Y") - 1)
# Create Month column
solar$month.n <- as.numeric(sapply(strsplit(solar$Date, "/"), "[",1)) - 1
solar$month.n[solar$month.n == 0] <- 12
solar$month <-  as.factor(sprintf("%02d",solar$month.n))
# Different seasonality for Solar and Non Solar
sn <- c(0,0,0,0,0,1,1,1,1,0,0,0) 
sy <- c(1,1,0,0,0,0,0,0,0,0,1,1) 
solar$season <- sapply(1:length(solar$Solar), function(i) ifelse(solar$Solar[i] == "N",  sn[solar$month.n[i]], sy[solar$month.n[i]])) + 1




# Explore Correlation
acf(solar$PowerBill[solar$Solar == "N"])
acf(solar$PowerBill[solar$Solar == "Y"])

#fit peak seasson ar model
# effect for peak season
fitar.ps <- gls(PowerBill ~ -1 + as.factor(season):Solar, correlation = corARMA(form=~rtime|Solar,p=1,q=0),data=solar,method="ML")
summary(fitar.ps)
plot(predict(fitar.ps),type="l")

str(intervals(fitar.ps))
coef.ints <- intervals(fitar.ps)$coef
corr.ints <- intervals(fitar.ps)$corStruct
resid.ints <- intervals(fitar.ps)$sigma
all.ints <- rbind(coef.ints,corr.ints,resid.ints)

xtable::xtable(all.ints)

#to do list -matt
# model assumptions after decorrelating the data
# make table of coefficients and uncertainties
# discussion of Rsquared, I can include this with brandon's results

#
#decorrelate model to check assumptions
#

#make R matrix
n.sn <- sum(solar$Solar == "N")
n.sy<- sum(solar$Solar == "Y")
sigma2 <- fitar.ps$sigma^2
phi <- coef(fitar.ps$modelStruct$corStruct,unconstrained=FALSE)

w <- "N"
n <- n.sn
k <- n.sy 
R <- diag(k+n)
R <- sigma2*phi^(abs(row(R) - col(R)))

my.X<- model.matrix(fitar.ps,data=solar)
dim(my.X)

#de-correlate model with variance matrix from below
# L.inv * Y ~ N(L.inv * X * beta, L.inv * L * t(L) * t(L.inv))
L<- t(chol(R))
Ystar <- solve(L) %*% solar$PowerBill

XX.star <- solve(L) %*%  my.X
decorr.mod <- lm(Ystar ~ XX.star)
summary(decorr.mod)
#residuals
plot(decorr.mod$fitted.values,decorr.mod$residuals,main='Fitted Values vs. Residuals')
abline(h=0,col='red')
hist(decorr.mod$residuals,prob=T,main='Histogram of Residuals')
curve(dnorm(x,0,sd(decorr.mod$residuals)),col='red',add=T)


pdf('residplot1.pdf')
plot(decorr.mod$fitted.values,decorr.mod$residuals,
     main='Fitted Values vs. Residuals')#,xlab='Fitted Values',ylab='Residuals')
abline(h=0,col='red')
dev.off()
pdf('residhist1.pdf')
hist(decorr.mod$residuals,prob=T,
     main='Histogram of Residuals')#, xlab='Residuals')
curve(dnorm(x,0,sd(decorr.mod$residuals)),col='red',add=T)
dev.off()

# #linearity
# library(car)
# avPlots(decorr.mod)










