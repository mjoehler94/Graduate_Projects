
###############
# Matt Oehler #
# Stat 536    #
# Credit EDA  #
###############


# Libraries
library(MASS)
library(xtable)
library(car) #has vif

#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 1 Credit")
credit <- read.csv('creditdata.csv',header=T)
head(credit)

#scatter and boxplots
pairs(credit[,c(1:6,11)])
oldpar = par(no.readonly = TRUE)
par(mfrow=c(2,2))
plot(credit[,7],credit$Balance,ylab='Balance',main='Gender')
plot(credit[,8],credit$Balance,ylab='Balance',main='Student')
plot(credit[,9],credit$Balance,ylab='Balance',main='Married')
plot(credit[,10],credit$Balance,ylab='Balance',main='Ethnicity')
par(oldpar)

#make table of summary statistics
summary(credit)
xtable(summary(credit[,c(1:6,11)]))#quant variables


#fit multiple linear regression model
model <- lm(Balance~., data=credit)

#check assumptions of the model

vif(model)#variance inflation factor
avPlots(model)

hist(model$residuals,freq=F)
curve(dnorm(x,0,sd(model$residuals)),col='red',lwd=2,add=T)

plot(model$fitted.values,model$residuals,
     main='Residual Plot')
abline(h=0,col='red')

#qqplot and other residual plots
plot(model)


###GGPLOT####
library(ggplot2)
library(GGally)

# ggpairs(credit)#huge
ggpairs(credit[,c(1:6,11)]) #quantitative variables

#boxplots
ggplot(credit,aes(x=Gender,y=Balance,fill=Gender)) + geom_boxplot()
ggplot(credit,aes(x=Student,y=Balance,fill=Student)) + geom_boxplot()
ggplot(credit,aes(x=Married,y=Balance,fill=Married)) + geom_boxplot()
ggplot(credit,aes(x=Ethnicity,y=Balance,fill=Ethnicity)) + geom_boxplot()

#model diagnostic plots

ggplot(model,aes(x=.fitted,y=.resid)) + geom_point() +
  geom_hline(yintercept=0, col="red", linetype="dashed")
ggplot(model,aes(x=.resid))+geom_histogram()

