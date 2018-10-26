
#EDA for hepatitis data
dat = read.csv("hep_clean.csv",header=T)[,-1]
head(dat)
dim(dat)

#leave out categorical data
hepdat <- dat[,c('Age','Bilirubin','AlkPhosphate','Sgot','AlbuMin','ProTime')]
apply(hepdat,2,hist)#make histograms of evry variable

library(xtable)
print(xtable(head(hepdat)))

rem.vals <- dat[complete.cases(dat),]
dim(rem.vals)


### Useful Packages ###

library(mvtnorm) #for random generating
library(norm) #for em.norm
library(mvnmle) #for mle of mvn
#######as written in package documentation
data(mdata)
s <- prelim.norm(mdata) #do preliminary manipulations
thetahat <- em.norm(s) #compute mle
getparam.norm(s,thetahat,corr=TRUE)$r #look at estimated correlations
getparam.norm(s,thetahat,corr=F) #get means and covariance

###now apply this to my randomly generated data
#generate random MVN data set
n <- 50
test <- rmvnorm(n,mean=c(0,1,2,4,5))
test.orig <- test
#randomly delete some of the data
miss.amount <- 0.2 #percentage of data to remove
rand.miss <- sample(c(1:length(test)),length(test) %/% (1/miss.amount))
test[rand.miss] <- NA
# test <- data.frame(test)
sum(is.na(test))#number of missing values

my.s <- prelim.norm(test)
my.estimates <- em.norm(my.s,showits=T) 

getparam.norm(my.s,my.estimates,corr=F)$mu #get mean vector
getparam.norm(my.s,my.estimates,corr=F)$sigma #get covariance matrix
getparam.norm(my.s,my.estimates,corr=TRUE)$r #look at estimated correlations


#MLE estimates of data after removing missing values
no.miss <- test[complete.cases(test),] #dataset after removing missing vals
#Get maximum likelihood estimates (after removing missing values)
mlest(test[complete.cases(test),])$muhat #mean vector
mlest(test[complete.cases(test),])$sigma #covariance matrix


