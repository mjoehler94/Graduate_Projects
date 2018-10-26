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


