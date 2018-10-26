###Gene R Code
## LASSO AND RIDGE REGRESSION

#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 3 Gene Expression/Gene Report")
genes <- read.table('GeneData.txt',header=T)
genes[1:5,1:5]


#ridge regression and the lasso (page 251)
library(glmnet)

#the '[,-1]' removes the intercept from the model matrix
x <- model.matrix(Malignant ~ ., data = genes)[,-1]
y <- genes$Malignant


#### MAKE TEST AND TRAIN SETS ###
test.ind <- sample(1:nrow(x),ceiling(nrow(x)/3))
y.test <- y[test.ind]
y.train <- y[-test.ind]
x.test <-  x[test.ind,]
x.train <- x[-test.ind,]

#alpha parameter dictates model type (0 == Ridge | 1 == LASSO)

#used for lambda parameter. This is the shrinkage parameter.
grid <- 10^seq(10,-2,length=100)
grid

#RIDGE (doesn't do variable selection)
#note that glm standardizes variables automatically
ridge.mod <- glmnet(x,y,alpha=0,lambda = grid)

#matrix of coefficients for each value of lambda
coef(ridge.mod)
dim(coef(ridge.mod))
#vvv for a single value of lambda
coef(ridge.mod)[,50]




##fit train data and validate with test data
ridge.mod2 <- glmnet(x.train,y.train,alpha=0,lambda = grid,
                     thresh = 1e-12)
#predicted values (note: s represents lambda)
ridge2.pred <- predict(ridge.mod2,s=4,newx=x.test)#try s=4 and s=1e9
mean((ridge2.pred-y.test)^2)


#rather than picking a lambda value by hand we will use cross-validation
cv.out <- cv.glmnet(x.train,y.train,alpha=0)
plot(cv.out,xlim=c(0,6))

#minimum lambda is best (maybe?)
bestlam <- cv.out$lambda.min
bestlam
abline(v=log(bestlam),col='blue')

#mse is best lambda
ridge2.pred <- predict(ridge.mod2,s=bestlam,newx=x.test)#try s=4 and s=1e9
mean((ridge2.pred-y.test)^2)

#ridge regression doesn't perform variable selection
#so none of the coefficients are zero (this could be problematic for us)


### LASSO MODEL ###
lasso.mod <- glmnet(x.train,y.train,alpha=1,lambda = grid)
plot(lasso.mod)

#cross validation for tuning parameter
cv.out <- cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out,xlim=c(-6,-1.5))

bestlam <- cv.out$lambda.min
bestlam
log(bestlam)
abline(v=log(bestlam),col='blue')


#assess error
lasso.pred <- predict(lasso.mod,s=bestlam,newx=x.test)
#MSE
mean((lasso.pred-y.test)^2)

#see coefs of best model
out <- glmnet(x,y,alpha=1,lambda=grid)
lasso.coefs <- predict(out,type = 'coefficients',s=bestlam)
lasso.coefs
lc <- lasso.coefs[lasso.coefs != 0]
lc
which(lasso.coefs != 0)
#this will do variable subset selection for us. 

#calculate R^2 by hand
#stack overflow1
cv.out <- cv.glmnet(x,y,alpha=1)
cf <- coef(cv.out, s = "lambda.1se")
i <- which(cv.out$lambda == cv.out$lambda.1se)
e <- cv.out$cvm[i]
r2 <- 1 - e/var(y)
r2

#stack overflow2
rx <- as.matrix(genes[,-1])
cc2 <- as.matrix(cf[-1,]) #remove intercept row
preds <- rx %*% cc2
err <- preds - genes$Malignant
r2b <-1-var(err)/var(genes$Malignant)
r2b



#linearity 
#sample covariate and look at scatter plots



###Bootstrap for lasso

B <- 1000 #Bootstrap iters
betahats <- matrix(NA,nrow=B,ncol=ncol(x))
dim(betahats)
for (b in 1:B){
  k <- nrow(x)
  vals <- sample(1:k,k,replace = T)
  boot.fit <- glmnet(x[vals,],y[vals],alpha=1,lambda=bestlam)
  boot.coefs <- predict(boot.fit,type='coefficients',s=bestlam)
  betahats[b,] <- t(boot.coefs)[1,-1]#-1 in index removes intercept
}
betahats[1:20,1:4]
dim(betahats)


ints <- apply(betahats,2,quantile,probs = c(0.025,0.975))
dim(ints)

ints[,1:10]
which(apply(ints,2,function(x) any(x==0))==F)
sum(apply(ints,2,function(x) any(x==0))==F)

apply(betahats,2,quantile,0.025)
quantile(betahats[,1],c(.025,.975))







##### BOOK DATA #####

library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
head(Hitters)

sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

xbook=model.matrix(Salary~.,Hitters)[,-1]
xbook[1:3,]
xbook1 = model.matrix(Salary~.,Hitters)
xbook1[1:3,]
colnames(xbook)
names(Hitters)
ybook=Hitters$Salary

###sols project
median.income <- c(58909,77734,58432,48869,67846,58856,83476,41901,55733,63812,53006,70093)

dr <- c(
14	, 
13	, 
19	, 
14.6, 
13	, 
14	, 
15	, 
17	, 
15	, 
17	, 
14	, 
14	)

er <- c(
46.2,
66.4,
39.9,
53.9,
62.3,
52.9,
67.3,
52.5,
63.1,
43.5,
50.1,
56.6)




