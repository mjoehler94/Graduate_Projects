setwd("/Users/Christiandavis/Documents/Stat/Graduate Work/Stat 536/Nonlinear Regression")

water <- read.table("AgWater.txt", header = TRUE)
head(water)
plot(water$cwsi, water$swc, xlab = "CWSI", ylab = "SWC")

## local regression ## 
water.lm <- loess(swc ~ cwsi, data = water, family = "gaussian")
sum.water <- summary(water.lm)
preds <- predict(water.lm, data.frame(cwsi = seq(0.01, 1, by = 0.01)), se = TRUE)
lines(seq(0.01,1,by = 0.01), preds$fit, col = "red")
## confidence intervals
lines(seq(0.01,1,by = 0.01), preds$fit + (qt(0.975,77)*preds$se.fit), col = "blue", lty = 2)
lines(seq(0.01,1,by = 0.01), preds$fit - (qt(0.975,77)*preds$se.fit), col = "blue", lty = 2)
## prediction intervals 
lines(seq(0.01,1,by = 0.01), preds$fit + (qt(0.975,77)*sqrt(preds$se.fit^2 + sum.water$s^2)), col = "purple", lty = 2)
lines(seq(0.01,1,by = 0.01), preds$fit - (qt(0.975,77)*sqrt(preds$se.fit^2 + sum.water$s^2)), col = "purple", lty = 2)


## b-splines cubic splines 
library(splines)
fit <- lm(swc ~ ns(cwsi,df = 6), data = water)
preds <- predict(fit, data.frame(cwsi = seq(0.01, 1, by = 0.01)), se = TRUE)

plot(water$cwsi, water$swc, xlab = "CWSI", ylab = "SWC")
lines(seq(0.01,1,by = 0.01), preds$fit, col = "red")
## confidence intervals
lines(seq(0.01,1,by = 0.01), preds$fit + (qt(0.975,77)*preds$se.fit), col = "blue", lty = 2)
lines(seq(0.01,1,by = 0.01), preds$fit - (qt(0.975,77)*preds$se.fit), col = "blue", lty = 2)
## prediction intervals 
lines(seq(0.01,1,by = 0.01), preds$fit + (qt(0.975,77)*sqrt(preds$se.fit^2 + sum.water$s^2)), col = "purple", lty = 2)
lines(seq(0.01,1,by = 0.01), preds$fit - (qt(0.975,77)*sqrt(preds$se.fit^2 + sum.water$s^2)), col = "purple", lty = 2)


fit <- lm(swc ~ ns(cwsi,knots = c(0.2,0.4,0.6,0.8)), data = water)
preds <- predict(fit, data.frame(cwsi = seq(0.01, 1, by = 0.01)), se = TRUE)


## exploring the data 
plot(water$cwsi, water$swc, xlab = "CWSI", ylab = "SWC")


## cross validating to number of knots ##
bias <- matrix(NA, nrow = 1000, ncol= 10)
mse <- matrix(NA, nrow = 1000, ncol= 10)
RSS <- matrix(NA, nrow = 1000, ncol= 10)
n <- dim(water)[1]

for(i in 1:10){
  for(cv in 1:1000){
    test.obs <- sample(1:n,10)
    test.data <- water[test.obs,]
    training.data <- water[-test.obs,]
    if(i == 1){
      training.lm <- lm(swc ~ cwsi, data = training.data)
    } else {
      training.lm <- lm(swc ~ ns(cwsi,df = i), data = training.data)
    }
    test.preds <- predict(training.lm, newdata=test.data)
    bias[cv,i-1] <- mean(test.preds-test.data$swc)
    mse[cv,i-1] <- mean((test.preds-test.data$swc)^2)
    RSS[cv,i-1] <- sum((test.data$swc-test.preds)^2)
  } 
}

plot(1:10,apply(mse,2,mean), ylab = "MSE", xlab = "Degrees of Freedom", type = "b")
plot(1:10,apply(bias,2,mean), ylab = "Bias", xlab = "Degrees of Freedom", type = "b")
plot(1:10,apply(RSS,2,mean), ylab = "Residual Sum of Squares", xlab = "Degrees of Freedom",type = "b")

## model
water.fit <- lm(swc ~ ns(cwsi,df = 3), data = water)
sum.fit <- summary(water.fit)
sum.fit

## testing assumptions 
with(water.fit, plot(fitted.values, residuals,pch = 20, col = "black", main = "Fitted Values Vs. Residuals Plot ",
                   xlab = "Fitted Values", ylab = "Residuals"))
abline(h = 0, lwd = 2, col = "red")


library(MASS)
stand.res <- stdres(water.fit)
hist(stand.res, main = "Histogram of Standard Residuals", xlab = "Standard Residuals")

qqnorm(stand.res)
qqline(stand.res)

preds <- predict(water.fit, data.frame(cwsi = seq(0.01, 1, by = 0.01)), se = TRUE)

## plotting fitted line and CI and PI
plot(water$cwsi, water$swc, xlab = "CWSI", ylab = "SWC")
lines(seq(0.01,1,by = 0.01), preds$fit, col = "red")
## confidence intervals
lines(seq(0.01,1,by = 0.01), preds$fit + (qt(0.975,77)*preds$se.fit), col = "blue", lty = 2)
lines(seq(0.01,1,by = 0.01), preds$fit - (qt(0.975,77)*preds$se.fit), col = "blue", lty = 2)
## prediction intervals 
lines(seq(0.01,1,by = 0.01), preds$fit + (qt(0.975,77)*sqrt(preds$se.fit^2 + sum.water$s^2)), col = "purple", lty = 2)
lines(seq(0.01,1,by = 0.01), preds$fit - (qt(0.975,77)*sqrt(preds$se.fit^2 + sum.water$s^2)), col = "purple", lty = 2)

########### cross validation #################
bias <- rep(NA, 1000)
rpmse <- rep(NA, 1000)
n <- dim(water)[1]
for(cv in 1:1000){
  test.obs <- sample(1:n,ceiling(n*0.1))
  test.data <- water[test.obs,]
  training.data <- water[-test.obs,]
  training.lm <- lm(swc ~ ns(cwsi,df = 3), data = training.data)
  test.preds<- predict.lm(training.lm, newdata=test.data)
  bias[cv] <- mean(test.preds-test.data$swc)
  rpmse[cv] <- sqrt(mean((test.preds-test.data$swc)^2))
}
hist(bias)
mean(bias)
hist(rpmse)
mean(rpmse)



coverage <- rep(NA,1000)
piw <- rep(NA,1000)
for(cv in 1:1000){ 
  test.obs2 <- sample(1:n, ceiling(n*0.1))  
  test.data2 <- water[test.obs2,]
  training.data2 <- water[-test.obs2,]
  training.lm2 <- lm(swc ~ ns(cwsi,df = 3), data = training.data2)
  test.preds2 <- predict.lm(training.lm2, newdata = test.data2, interval = "prediction", level = 0.95)
  coverage[cv] <- mean(test.data2$swc > test.preds2[,2] & test.data2$swc < test.preds2[,3])
  piw[cv] <- mean(test.preds2[,3] - test.preds2[,2])
}
hist(coverage)
mean(coverage)
hist(piw)
mean(piw)

########### predict ###############
water_data <- data.frame(cwsi = 0.4)

preds <- predict.lm(water.fit ,newdata=water_data ,interval="prediction",level=.95)
preds
