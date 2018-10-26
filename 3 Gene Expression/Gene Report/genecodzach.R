############## Gene Analysis Script ################
## Performs LASSO regression on gene expression data

#### prep data ####
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 3 Gene Expression/Gene Report")
gene <- read.table("GeneData.txt",header = TRUE)
library(glmnet)
y <- gene$Malignant
#x <- model.matrix(Malignant~.,gene)[,-1] # same thing
x <- as.matrix.data.frame(gene[,-1])
x <- (apply(x,2,scale))## center and scale
grid <- 10^seq(10,-2, length =100)

#### exploratory code ####
# lasso.fit <- glmnet(x = x, y = y, alpha = 1, lambda = grid)
# plot(lasso.fit) #matt
#dim(coef(lasso.fit))

#### cross validate to estimate lambda ####
set.seed(1234)
cv.out <- cv.glmnet(x = x, y = y, alpha = 1) #plot
# cv.out <- cv.glmnet(x = x, y = y, alpha = 1,lambda=grid) #matt
lam <- cv.out$lambda.1se ########## change to min for the other one

#### Fit model to data ####
lasso.fit <- glmnet(x = x, y = y, alpha = 1, lambda = lam, standardize = FALSE)
non.zero <- which(coef(lasso.fit)!=0)
lasso.coef <- coef(lasso.fit)[non.zero,]

#### Bootstraping ####
#now that we have coeficients, we need standard errors
#we resample data, recompute estimates, and repeat
#at the end we create CI's and find significance
boots <- 1000
coef.save <- NULL
n <- dim(gene)[1]
newx <- x[,non.zero[-1]-1] #this compensates for the added intercept in non.zero
for(i in 1:boots){
  new.ind <- sample(n, replace = TRUE)
  new.fit <- glmnet(x = newx[new.ind,], y = y[new.ind], alpha = 1, lambda = lam, standardize = FALSE)
  coef.save <- rbind(coef.save,as.numeric(coef(new.fit)))
}

#quantile intervals
quants <- apply(coef.save,2,quantile,c(0.025, 0.975))
rbind(quants[1,],lasso.coef,quants[2,])
which(apply(quants,2,function(x) all(x != 0)))

#centered intervals
ci <- 2*rbind(lasso.coef,lasso.coef) - quants
rbind(ci[2,],lasso.coef,ci[1,])  
which(apply(ci,2,function(x) prod(x) > 0 ))

##### R2 Calculation #####
preds <- as.numeric(cbind(1,newx)%*%lasso.coef)

(r2 <- 1 - sum((y - preds^2))/sum((y - mean(y)^2)))#check this with zach
(r2 <- 1 - sum((y - preds)^2)/sum((y - mean(y))^2))#matt

##r2 #matt
lasso.response <- predict(lasso.fit,type = 'response',s=lam,newx=x)
SS_res <- sum((y - lasso.response)^2)
SS_tot<- sum((y - mean(y))^2)
m.r2 <- 1-(SS_res / SS_tot)
m.r2

#mse #matt
test.ind <- sample(1:nrow(x),10)#ceiling(nrow(x)/8))
y.test <- y[test.ind]
y.train <- y[-test.ind]
x.test <-  x[test.ind,]
x.train <- x[-test.ind,]
dim(x.train)
dim(x.test)

#assess error
lasso.train <- glmnet(x.train,y.train,lambda = lam)#can we use this lambda?
lasso.pred <- predict(lasso.train,s=lam,newx=x.test)
#MSE
mse <- mean((lasso.pred-y.test)^2)
mse



### make tables #matt
library(xtable)
perf.tab<- rbind(MSE = mse,'R-squared' = m.r2)
xtable(perf.tab)

#table of lambdas
lam
min.lam <- cv.out$lambda.min
lam.tab <- rbind('Minimum' = min.lam,'1 Std. Error' = lam)
lam.tab
xtable(lam.tab)
  