#### GENE R CODE ###
#Pincipal component regression and Partial least squares



#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 3 Gene Expression/Gene Report")
genes <- read.table('GeneData.txt',header=T)
genes[1:5,1:5]

#the '[,-1]' removes the intercept from the model matrix
x <- model.matrix(Malignant ~ ., data = genes)#[,-1]
x[1:5,1:5]
y <- genes$Malignant

#### MAKE TEST AND TRAIN SETS ###

test.ind <- sample(1:nrow(genes),ceiling(nrow(genes)/3))
train.ind <- -test.ind
# y.test <- y[test.ind]
# y.train <- y[-test.ind]
# x.test <-  x[test.ind,]
# x.train <- x[-test.ind,]

#### PRINCIPAL COMPONENT REGRESSION
library(pls)

pcr.fit<- pcr(Malignant~.,data=genes,scale=TRUE,validation='CV')
summary(pcr.fit)

validationplot(pcr.fit,val.type = 'MSEP')


#using test and train
pcr.fit2<- pcr(Malignant~.,data=genes,subset=train.ind,
               scale=TRUE,validation='CV')
validationplot(pcr.fit2,val.type = c("RMSEP"))
validationplot(pcr.fit2,val.type = c( "MSEP"))
validationplot(pcr.fit2,val.type = c("R2"))





# sumsum(EV)/sum(EV)







#sample transforms
y_<- genes$Malignant
hist(y_)
trans <- log(y_/(1-y_))
trans
hist(trans,pro=T)
curve(dnorm(x,0,sd(trans)),col='red',add=T)

##### PARTIAL LEAST SQUARES ###

pls.fit<- plsr(Malignant~.,data=genes,subset = train.ind, scale=T,validation='CV')
class(summary(pls.fit))

validationplot(pls.fit,val.type = 'RMSEP',ylim=c(0,500))

x_ <- model.matrix(Malignant~.,data=genes)[,-1]
x_[test.ind,1:5]
pls.pred <- predict(pls.fit,x_[test.ind,],ncomp=10)
pls.pred
y <- genes$Malignant

mean((pls.pred - y[test.ind])^2)










####book data
library(ISLR)
names(Hitters)
Hitters=na.omit(Hitters)
dim(Hitters)
head(Hitters)

pcr.book <- pcr(Salary~.,data=Hitters,scale=TRUE,validation='CV')
validationplot(pcr.book,val.type = 'MSEP')