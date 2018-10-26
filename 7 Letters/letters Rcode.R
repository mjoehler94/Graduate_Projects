###svm lab

set.seed(1)
x <- matrix(rnorm(20*2),ncol=2)
y <- c(rep(-1,10),rep(1,10))

x[y==1,] <- x[y==1,]+1
x[y==1,]

plot(x[,2],x[,1],col=(3-y),xlim=c(-3,3))

dat = data.frame(x=x,y=as.factor(y))
library(e1071)

svmfit <- svm(y~.,data=dat,kernel='sigmoid',cost=10,scale=T)
summary(svmfit)

plot(svmfit,dat,xlim=c(-3,3))




### xtable 

#data description
namevec <- c(
'lettr ',
'x-box ',
'y-box ',
'width ',
'high  ',
'onpix ',
'x-bar ',
'y-bar ',
'x2bar ',
'y2bar ',
'xybar ',
'x2ybr ',
'xy2br ',
'x-ege ',
'xegvy ',
'y-ege ',
'yegvx '
)

desc <- c(
'capital letter (26 values from A to Z)'
,'horizontal position of box (integer)'
,'vertical position of box (integer)'
,'width of box (integer)'
,'height of box (integer)'
,'total # of pixels (integer)'
,'mean x of pixels in box (integer)'
,'mean y of pixels in box (integer)'
,'mean x variance (integer)'
,'mean y variance (integer)'
,'mean x y correlation (integer)'
,'mean of x * x * y (integer)'
,'mean of x * y * y (integer)'
,'mean edge count left to right (integer)'
,'correlation of x-ege with y (integer)'
,'mean edge count bottom to top (integer)'
,'correlation of y-ege with x (integer)'
)

xtable::xtable(cbind(namevec,desc))





















