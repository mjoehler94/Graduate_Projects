######################
# Gene Report R code #
# Stat 536           #
# Matt Oehler        #
######################

#clear workspace
rm(list=ls())

#### DATA ####
#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 3 Gene Expression/Gene Report")
genes <- read.table('GeneData.txt',header=T)
genes[1:5,1:5]


#note: there is one response variable and 5149 covariates
genes[,-1] <- scale(genes[,-1],center = T,scale = T)

## TRAIN AND TEST SET ##
#the '[,-1]' removes the intercept from the model matrix
x <- model.matrix(Malignant ~ ., data = genes)[,-1]
dim(x)
# x1<- model.matrix(Malignant ~ ., data = genes)
# dim(x1)
# x[1:5,1:5]
# x1[1:5,1:5]

#center and scale the x's
x <- scale(x,center=T,scale=T)

y <- genes$Malignant

#### MAKE TEST AND TRAIN SETS ###
set.seed(123)
test.ind <- sample(1:nrow(x),10)#ceiling(nrow(x)/8))
y.test <- y[test.ind]
y.train <- y[-test.ind]
x.test <-  x[test.ind,]
x.train <- x[-test.ind,]
dim(x.train)
dim(x.test)

#library for lasso/ridge/elastic net
library(glmnet)

#used for lambda parameter. This is the shrinkage parameter.
grid <- 10^seq(10,-2,length=100)
grid

#alpha dictates model type (0 == Ridge | 1 == LASSO)

### LASSO MODEL ###
lasso.mod <- glmnet(x.train,y.train,alpha=1,lambda = grid)
# plot(lasso.mod) #use the plot from the main model

#cross validation for tuning parameter
cv.out <- cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out)#,xlim=c(-6,-1.5))

bestlam <- cv.out$lambda.min
bestlam
log(bestlam)
abline(v=log(bestlam),col='blue')
#most regularized lambda that is within 1se of the minimum
lamb.1se <- cv.out$lambda.1se
lamb.1se
abline(v=log(lamb.1se),col='green')

#assess error
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x.test)
#MSE
mse <- mean((lasso.pred-y.test)^2)
mse


#see coefs of best model (uses both test and train data)
out <- glmnet(x,y,alpha=1,lambda=grid)
pdf('lassoplot2.pdf')
plot(out)
dev.off()
lasso.coefs <- predict(out,type = 'coefficients',s=lamb.1se)
lasso.coefs
non.zero <- which(lasso.coefs != 0)
lc <- lasso.coefs[non.zero,]
lc
length(lc)
#this will do variable subset selection for us.

#calculate R^2
# 1 - (SS_res / SS_tot)
lasso.response <- predict(out,type = 'response',s=bestlam,newx=x)
SS_res <- sum((y - lasso.response)^2)
SS_tot<- sum((y - mean(y))^2)
r2 <- 1-(SS_res / SS_tot)



#make a residuals plot
fit.vals <- predict(out,type='response',s=bestlam,newx=x)
resids <- y-fit.vals
plot(fit.vals,resids)

hist(resids,prob=T)
curve(dnorm(x,0,sd(resids)),col='red',add=T)

###Bootstrap for lasso

B <- 1000 #Bootstrap iters
# betahats <- matrix(NA,nrow=B,ncol=ncol(x))
betahats <- matrix(NA,nrow=B,ncol=length(non.zero))
dim(betahats)
for (b in 1:B){
  k <- nrow(x)
  vals <- sample(1:k,k,replace = T)
  boot.fit <- glmnet(x[vals,non.zero],y[vals],alpha=1,lambda=bestlam)
  boot.coefs <- predict(boot.fit,type='coefficients',s=bestlam)
  betahats[b,] <- t(boot.coefs)[1,-1]#-1 in index removes intercept
}
betahats[1:20,1:10]
dim(betahats)

apply(betahats,2,quantile,probs = c(0.025,0.975))
apply(betahats,2,quantile,0.025)
quantile(betahats[,1],c(.025,.975))

ints <- apply(betahats,2,quantile,probs = c(0.025,0.975))
dim(ints)

ints[,1:10]
which(apply(ints,2,function(x) any(x==0))==F)
# sum(apply(ints,2,function(x) any(x==0))==F)

rbind(lasso.coefs[non.zero,],ints[1,],ints[2,])

############ PLOTS AND FIGURES ###################
#make table for performance evaluation
library(xtable)
perf.tab <- rbind(MSE = mse,'R-squared' = r2)
xtable(perf.tab)

#hist of Malignancy
pdf('malighist.pdf')
hist(genes$Malignant,xlab = 'Score',main = 'Histogram of Malginancy',ylab='')
dev.off()

###########################################################

#what happens if I transform the response
t.out <- glmnet(x,logit(y),alpha=1,lambda=grid)
cv.out.t <- cv.glmnet(x.train,logit(y.train),alpha=1)
plot(cv.out.t)#,xlim=c(-6,-1.5))

t.bestlam <- cv.out.t$lambda.min
t.lam.1se <- cv.out.t$lambda.1se
fit.vals.t <- predict(t.out,type='response',s=t.bestlam,newx=x)
resids.t <- logit(y) - fit.vals.t
plot(fit.vals.t,resids.t)

hist(resids.t,prob=T,ylim=c(0,1.4))
curve(dnorm(x,0,sd(resids.t)),col='blue',add=T)

#assess error
lasso.pred.t <- predict(t.out,s=t.bestlam,newx=x.test)
#MSE
mse.t <- mean((lasso.pred.t-logit(y.test))^2)
mse.t

#regular mse
out.pred <- predict(out,s=bestlam,newx=x.test)
mse.out <- mean((out.pred-y.test)^2)
mse.out









