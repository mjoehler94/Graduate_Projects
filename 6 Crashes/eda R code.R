
#Car Crashes EDA Code

## NOTES
#hour = 0-23
#typ_int = type of intersection
#rest_use = restrint system used
#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 6 Crashes")
crash.raw <- read.table('Crash.txt',header=T)
crash <- crash.raw[,c(2:8,10:14,9)]
head(crash)
dim(crash)
names(crash)

## EDA ##
summary(crash)
#tabulations with or without severity
for(i in names(crash)){
  print(i)
  print(table(crash$SEVERITY,crash[,i]))#with severity
  # print(table(crash[,i]))#without severity
  cat('\n')
}

#variables to fix
#airbag <- make deployed or not deployed
my_airbag <- ifelse(crash$AIR_BAG==20,0,1)
#vsurcond <- 0: dry, 1: wet, 2: snow or 10,  4: 
mk_surf <- function(vals){
  new_vals <- rep(NA,length(vals))
  new_vals[which(vals==1)] <- 'dry'#dry
  new_vals[which(vals %in% c(2,6))] <- 'wet'#wet
  new_vals[which(vals %in% c(3,10))] <- 'snow'#snow or slush
  new_vals[which(vals==4)] <- 'ice' #ice/frost
  new_vals[which(vals %in% c(5,8,11))] <- 'other' #mud,dirt,gravel,sand,other
  if(any(is.na(new_vals))){stop("error")}
  else{return(new_vals)}
}
my_surface <- mk_surf(crash$VSURCOND)

#rest use <- make binary 0 if restraint, 1 if no restraint
my_restraint <- ifelse(crash$REST_USE==7,1,0)

#typ int make factor of 0 non int, 1 some kind of int
my_intersection <- ifelse(crash$TYP_INT==1,0,1)

#vnumlane <- combine last two classes since small group
my_numlane <- ifelse(crash$VNUM_LAN>6,6,crash$VNUM_LAN)

#valign <- keep as a factor

#weather
mk_weather <- function(vals){
  new_vals <- rep(NA,length(vals))
  new_vals[which(vals ==1)] <- 'clear'
  new_vals[which(vals %in% c(2,12))] <- 'rain'
  new_vals[which(vals ==3)] <- 'hail'
  new_vals[which(vals ==4)] <- 'snow'
  new_vals[which(vals ==5)] <- 'fog'
  new_vals[which(vals %in% c(6,7,11))] <- 'windy'
  new_vals[which(vals == 10)] <- 'cloudy'
  if(any(is.na(new_vals))){stop("error")}
  else{return(new_vals)}
}
my_weather <- mk_weather(crash$WEATHER)

#hour
my_time <- ifelse(crash$HOUR > 21 | crash$HOUR < 7,'Night','Day')

#ALCOHOL <-  change for interpretability
my_alcohol <- ifelse(crash$ALCOHOL == 1,1,0)#'yes','no')
# my_alcohol <- ifelse(crash$ALCOHOL == 1,'yes','no')


#vtrafway leave as factor
#light condition is factor

#make clean data frame
crash$my_airbag <- my_airbag
crash$my_surface <- my_surface
crash$my_restraint <- my_restraint
crash$my_intersection <- my_intersection
crash$my_numlane <- my_numlane
crash$my_weather <- my_weather
crash$my_time <- my_time
crash$my_alcohol <- my_alcohol

##look at plots of raw and my variables
for(i in 1:ncol(crash)){
  nam <- names(crash)[i]
  scatter.smooth(crash[[i]],crash$SEVERITY,xlab=i,main=nam)
  cat(i,' ')
}


#identify columns to keep (13 including response)
keeps <- c("LGT_COND" ,"VTRAFWAY" , "VSPD_LIM",  "VALIGN" , "my_airbag",
           "my_surface","my_restraint", "my_intersection" ,"my_numlane", "my_weather",
           "my_time","my_alcohol","SEVERITY" )

#specify factors as necessary
#surface, weather, alcohol, time, are already characters
#numlane, speedlim,  should stay numeric
temp.clean <- crash[,keeps]
temp.clean$LGT_COND <- as.factor(temp.clean$LGT_COND)
temp.clean$VTRAFWAY <- as.factor(temp.clean$VTRAFWAY)
temp.clean$VALIGN <- as.factor(temp.clean$VALIGN)
temp.clean$my_airbag <- as.factor(temp.clean$my_airbag)
temp.clean$my_surface <- as.factor(temp.clean$my_surface)
temp.clean$my_restraint <- as.factor(temp.clean$my_restraint)
temp.clean$my_intersection <- as.factor(temp.clean$my_intersection)
temp.clean$my_weather <- as.factor(temp.clean$my_weather)
temp.clean$my_time <- as.factor(temp.clean$my_time)
# temp.clean$my_alcohol <- as.factor(temp.clean$my_alcohol)
temp.clean$SEVERITY <- temp.clean$SEVERITY

sapply(temp.clean,class)

clean.dat <- temp.clean


#make barplots for all of the new variables

for(i in names(clean.dat)){
  print(i)
  barplot(table(clean.dat$SEVERITY,clean.dat[,i]),main=i)#with severity
  # print(table(crash[,i]))#without severity
  cat('\n')
}





# plot(tennis$DBF,jitter(tennis$Result,amount = .5))
# scatter.smooth(tennis$DBF,tennis$Result)

plot(crash$ALCOHOL,jitter(crash$SEVERITY,amount = .25))
scatter.smooth(crash$ALCOHOL,crash$SEVERITY)
scatter.smooth(crash$VSPD_LIM,crash$SEVERITY)
scatter.smooth(crash$VSURCOND,crash$SEVERITY,
               cex=c(table(crash$VSURCOND)/1000))
scatter.smooth(crash$VSURCOND,crash$SEVERITY)


###I only need to check that scatter smooth plots for quantitative variables
### so only for speed limit in my case!!!


for(i in 1:ncol(clean.dat)){
  nam <- names(clean.dat)[i]
  scatter.smooth(clean.dat[[i]],clean.dat$SEVERITY,xlab=i,main=nam)
  cat(i,' ')
}
#my_rest
scatter.smooth(jitter(as.numeric(clean.dat[[7]]),0.1),
               jitter(clean.dat$SEVERITY,0.1))

for(i in 8:ncol(clean.dat)){
  nam <- names(clean.dat)[i]
  scatter.smooth(clean.dat[[i]],clean.dat$SEVERITY,xlab=i,main=nam)
}

scatter.smooth(jitter(as.numeric(clean.dat[[13]]),0.1),
               jitter(clean.dat$SEVERITY,0.1))


boxplot(clean.dat$SEVERITY,clean.dat$LGT_COND)

#boxplots
for(i in 1:13){
  nam <- names(clean.dat)[i]
  boxplot(clean.dat$SEVERITY,clean.dat[[i]],main=nam)
}

#scatters smooths
for(i in 1:13){
  nam <- names(clean.dat)[i]
  scatter.smooth(clean.dat$SEVERITY,clean.dat[[i]],main=nam)
}

#TO DO LIST:
#-consider interactions?
#-add a spline on light conditions, figure out why alcohol is weird
#-add cross tabulations to paper for my_time
# make table of covariates used for the analysis


#histograms
for(i in 1:13){
  nam <- names(clean.dat)[i]
  hist(clean.dat[[i]],main=nam)
}

##keepers
scatter.smooth(clean.dat$ALCOHOL,clean.dat$SEVERITY,main='Alcohol')
pdf('speedscatter.pdf')
scatter.smooth(clean.dat$VSPD_LIM,clean.dat$SEVERITY,main='',
               xlab = 'Speed Limit',ylab='Severity')
dev.off()
scatter.smooth(clean.dat$my_numlane,clean.dat$SEVERITY,main='Alignment')
boxplot(clean.dat$SEVERITY,clean.dat$VSPD_LIM,main='Speed Limit')

#xtable
library(xtable)
xtable(table(clean.dat$SEVERITY))

###############################################################################

#cleaned data
head(clean.dat)


#variable selection to find best model
library(bestglm)
#takes a few minutes (6ish)
system.time(
  var.select<- bestglm(clean.dat,IC="BIC",method="exhaustive",family=binomial)
)
bestmod<-var.select$BestModel
summary(bestmod)
bestmod$coefficients

# bestmod <- glm(...)
bestmod <- glm(SEVERITY~VSPD_LIM+my_airbag+my_restraint+my_time+my_alcohol,
    data=clean.dat, family = binomial)
summary(bestmod)
#confidence intervals
confint(bestmod)
exp(confint(bestmod))
100*(exp(confint(bestmod))-1)

#make ci table (most interpretable)
ci.tab <- exp(cbind(confint(bestmod)[,1],(bestmod$coefficients),confint(bestmod)[,2]))
ci.tab
xtable(ci.tab)



#find the threshold and do a cross validation
pred.probs <- predict.glm(bestmod,type="response")
thresh <- seq(0,1,length=100)
misclass <- rep(NA,length=length(thresh))
mis.class <- c()
for(i in 1:length(thresh)){
  #If probability greater than threshold then 1 else 0 
  my.classification <- ifelse(pred.probs>thresh[i],1,0) 
  # calculate the pct where my classification not eq truth
  misclass[i] <- mean(my.classification!=clean.dat$SEVERITY)
}
#Find threshold which minimizes miclassification
threshval <- thresh[which.min(misclass)]
threshval

#this plots the number of misclassified predictions for different threshold values
#we pick the value that minimizes misclassifications
pdf('thresh.pdf')
plot(thresh,misclass,xlab="Threshold",ylab="Misclassifications",type = 'l',
     main='Picking a Threshold Value')
abline(v=threshval,lty=2,col='red')
dev.off()


#cross validation
iters <- 1000
acc.vec <- rep(NA,iters)
for (i in 1:iters){
  n.test <- ceiling(nrow(clean.dat)*.15)
  test.obs <- sample (1:nrow(clean.dat), n.test)
  test.data <- clean.dat[test.obs,]
  #everything else will be the train data
  train.data <- clean.dat[-test.obs,]
  train.mod <- glm(SEVERITY~VSPD_LIM+my_airbag+my_restraint+my_time+my_alcohol,
                   data=train.data, family = binomial)
  test.preds <- predict.glm(train.mod,newdata=test.data,type="response")
  #include the previously calculated threshold value
  test.class <- ifelse(test.preds>threshval,1,0)
  #prediction accuracy on test set
  acc.vec[i] <- mean(test.data$SEVERITY == test.class)
  if(i %% 100 == 0) {cat(i,'\n')}
}
mean(acc.vec)
#table of results
table(test.data$SEVERITY,test.class,dnn = c("Actual","Predicted"))


##### add a ROC Curve 
#overall accuracy
misclassified <- mean(test.class != test.data$SEVERITY)
print(paste('Accuracy',round(1-misclassified,4)))
# 
# #create confusion matrix
# #below is the format of the confusion matrix
# #         Predicted A | Predicted C
# # True A
# # True C
cm <- table(test.data$SEVERITY,test.class,dnn = c("Actual","Predicted"))
cm
# #the matrix gives us the amount of false positives, false negatives, etc
# 
# #percent of True Completed Users that were correctly predicted
sensitivity <- cm[4] / (cm[4]+ cm[2])
sensitivity
# 
# #Percent of True Abandoned users who were correctly predicted
specificity <- cm[1] / (cm[1] + cm[3])
specificity
# 
# #Positive Predicted Value
# #percent of values that were predicted completed that were correct
ppv <- cm[4] / (cm[4] + cm[3])
ppv
# #Negative Predicted Value
# #percent of values that were predicted abandoned that were correct
npv <- cm[1] / (cm[1] + cm[2])
npv

#make a result summary table
sum.names <- c('Sensitivity','Specificity','PPV','NPV' )
sum.tab <- rbind(sensitivity,specificity,ppv,npv)
xtable(sum.tab)



# 
# ###ROC, AUC, and Model Accuracy
# #plotting the ROC Curve
library(ROCR)
p <- test.preds
pr <- prediction(p,test.data$SEVERITY)
prf <- performance(pr,measure = "tpr",x.measure="fpr")
pdf('roc.pdf')
plot(prf,main="ROC Curve")
abline(a=0,b=1,lty=2)
dev.off()
#area under curve
auc <- performance(pr,measure = "auc")
auc <- auc@y.values[[1]]
auc

# tables and summarys for model fit on all of the data--------------------------
#table for the whole model
all.mod <- glm(SEVERITY~VSPD_LIM+my_airbag+my_restraint+my_time+my_alcohol,
               data=clean.dat, family = binomial)
all.preds <- predict.glm(all.mod,newdata=clean.dat[,-13],type="response")
all.class <- ifelse(all.preds>threshval,1,0)
# table(clean.dat$SEVERITY,all.class,dnn = c("Actual","Predicted"))
xtable(summary(all.mod))




# table(test.class,test.data$Result)

#prediction
# predict.log.odds <- predict.glm(bestmod,newdata=data.frame(DBF=7,NPA=4,NPW=55))
# pred.prob <- exp(predict.log.odds)/(1+exp(predict.log.odds)) 
# pred.pr

#-------------------------------------------------------------------
# best model with splines
# regular best mod
# bestmod <- glm(SEVERITY~VSPD_LIM+my_airbag+my_restraint+my_time+my_alcohol,
#                data=clean.dat, family = binomial)
# summary(bestmod)
library(splines)
spline.mod <- glm(SEVERITY~VSPD_LIM+my_airbag+my_restraint+my_time+my_alcohol,
                                 data=clean.dat, family = binomial)




################################################################################
## to do list
- add model assumption to model section
- add model assumptions to results section
- add chosen threshold to results section
- add cross validation to results
- add roc curve to results section
- write conclusion


