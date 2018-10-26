#################
# Matt Oehler   #
# Stat 536      #
# Credit Report #
#################


##################################################################################
### CODE FOR REPORT ##############################################################
##################################################################################

##Read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 1 Credit/Credit Project")
credit <- read.csv('creditdata.csv',header=T)
head(credit)

##NOTES:
# check ALL assumptions
# see if transformations are necessary
# variable subset selection
# look at student and income interaction
# validate predicitions
# validate prediction uncertainty (check the coverage)

## OUTLINE:
# backgroud and problem introduction
# 
# based on the data we decide to use multiple linear regression
#   - list the assumptions that we will need to verify

#EDA
###pairs plot
library(GGally)
pdf('creditpairs.pdf')
ggpairs(credit[,c(1:6,11)]) #quantitative variables
dev.off()
pairs(credit[,c(1:6,11)])

##VIF shows that 

library(car)
vif(lm(Balance~.,data=credit))

###box plots
pdf('creditboxplots.pdf')
par(mfrow=c(2,2))
plot(credit[,7],credit$Balance,ylab='Balance',main='Gender')
plot(credit[,8],credit$Balance,ylab='Balance',main='Student')
plot(credit[,9],credit$Balance,ylab='Balance',main='Married')
plot(credit[,10],credit$Balance,ylab='Balance',main='Ethnicity')
dev.off()

##model selection and make new data set
# we want to also look at student income interaction
# first see if those variables are still included with criterion
library(bestglm)
temp1 <- credit[,names(credit) != 'Limit']
#we use AIC since we are focused on prediction
var.select <- bestglm(temp1,IC = 'AIC',method='exhaustive')
var.select$Subsets
bestmod <- var.select$BestModel
summary(bestmod)
plot(bestmod)

my_data <- temp1[,c('Income','Rating','Age','Student','Balance')]
head(my_data)
#add interaction
my_mod <- lm(Balance~Student*Income+Rating+Age,data=my_data)
summary(my_mod)

#interaction of student and income
library(ggplot2)
ggplot(my_data,aes(x=Income,y=Balance,color=Student))+ geom_point() +
  ggtitle("Student/Income Interaction") + stat_smooth(method='lm',se=F)+ theme_bw()

qplot(x=Income,y=Balance,data=my_data,color=Student) + geom_smooth(method='lm') +
  ggtitle("Student/Income Interaction") + stat_smooth(method='lm',se=F)+ theme_bw()
qplot(x=Income,y=Balance,data=credit, facets = ~Student) + 
  geom_smooth(method = "lm")

#check assumptions of my model
#linearity
pdf('avplotscredit.pdf')
avPlots(my_mod)
dev.off()
vif(my_mod)
#independence

#normality
pdf('histofresiduals.pdf')
hist(my_mod$residuals,freq=F,main="Histogram of Residuals")
curve(dnorm(x,0,sd(my_mod$residuals)),col='red',lwd=2,add=T)
dev.off()

#equal variance
plot(my_mod)
pdf('equalvariance.pdf')
plot(my_mod$fitted.values,my_mod$residuals,
     main='Fitted Values vs. Residulas',
     xlab='Fitted Values',ylab = 'Residuals')
abline(h=0,col='red')
dev.off()

#diagnostic pdf
pdf('modeldiag.pdf')
par(mfrow=c(1,2))
hist(my_mod$residuals,freq=F,main="Histogram of Residuals")
curve(dnorm(x,0,sd(my_mod$residuals)),col='red',lwd=2,add=T)

plot(my_mod$fitted.values,my_mod$residuals,
     main='Fitted Values vs. Residulas',
     xlab='Fitted Values',ylab = 'Residuals')
abline(h=0,col='red')
dev.off()

#assess outliers with cooks distance
plot(my_mod$fitted.values,my_mod$residuals,
     main='Residual Plot')
abline(h=0,col='red')
cooks.distance(my_mod)
points(my_mod$fitted.values[cooks.distance(my_mod)>4/nrow(my_data)],
       my_mod$residuals[cooks.distance(my_mod)>4/nrow(my_data)],
       col='red')

#qqplot and other residual plots





# var.selection <-bestglm(modFarm,IC="BIC",method="exhaustive")
# var.selection$Subsets
# bestlm <- var.selection$BestModel
# summary(bestlm)
# plot(bestlm)
# confint(bestlm)





###CROSS VALIDATION
cross_validate <- function(data,model,test.size=0.3,nIters = 10000,coverage=F){
  #Parameters:
  #-----------
  # data = dataset
  # model = model to train on the data
  # test.size = proportion of values to be put into test.data
  #
  
  #split into test and train data sets
  rows <- nrow(data)
  test.index <- sample(1:rows,ceiling(rows*0.3))
  test.data <- data[test.index,]#test set
  train.data <- data[-test.index,]#train set
}

Iters <- 10000
coverage<- rep(NA,Iters)
bias <- rep(NA,Iters)
rpmse <- rep(NA,Iters)
piw<- rep(NA,Iters)
data <- my_data
#takes 30 seconds or so
for (i in 1:Iters){
  rows <- nrow(data)
  test.index <- sample(1:rows,ceiling(rows*0.3))
  test.data <- data[test.index,]#test set
  train.data <- data[-test.index,]#train set
  
  train.model <- lm(Balance~Income+Rating+Age+Student+Student:Income, data=train.data)
  test.predictions <- predict.lm(train.model,newdata = test.data, interval="prediction")
  
  bias[i] <- mean(test.predictions[,1]-test.data$Balance)
  coverage[i] <- mean(test.data$Balance > test.predictions[,2] &
                        test.data$Balance < test.predictions[,3])
  rpmse[i] <- sqrt(mean((test.predictions[,1] - test.data$Balance)^2))
  piw[i] <- mean(test.predictions[,3]-test.predictions[,2])
  
}
mean.cov <- mean(coverage)
mean.bias <- mean(bias )
mrpmse <- mean(rpmse)
mean.piw <- mean(piw)


mean.cov
mean.bias
mrpmse
mean.piw



##making tables and some figures
Variable <- c(
'Income', 
'Limit', 
'Rating',
'Cards',
'Age', 
'Education',
'Gender', 
'Student', 
'Married', 
'Ethnicity', 
'Balance') 


Description <- c(
'Card holders annual income in thousands.',
'Card holders credit limit.',
'Credit rating - similar to a FICO credit score but used internally by the company.',
'Number of open credit cards (including the current card) of the card holder.',
'Age of the card holder.',
'Years of education.',
'Gender of the card holder.',
'Card holder is a full-time student.',
'Card holder is married.',
'Card holders ethnicity (Caucasian, Asian, or African-American.)',
'Current credit card debt.')

desc.table <- cbind(Variable = Variable, Description = Description)
desc.table

#variable description table
library(xtable)
xtable(desc.table)

#model summary table
xtable(summary(my_mod))


#confidence interval table
ci <- confint(my_mod,type='confidence',level=.95)
xtable(ci)


##prediction interval table 
pi <- confint(my_mod,type='prediction',level=.95)
pi
xtable(pi)


mean.cov
mean.bias
mrpmse
# mean.piw

pred.names <- c('Coverage','Bias','RPMSE','Interval Width')
pred.res <- round(c(mean.cov,mean.bias,mrpmse,mean.piw),2)
pred.tab <- cbind(pred.names,pred.res)
pred.tab

xtable(pred.tab,row.names=F)



