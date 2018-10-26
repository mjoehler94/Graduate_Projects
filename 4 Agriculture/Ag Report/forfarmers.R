#!/usr/bin/env Rscript

# Use the following line in the commandline to run this script:
# Rscript --vanilla forfarmers.R .3

args = commandArgs(trailingOnly=TRUE)


# R script that farmers can run from the commandline


# setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 4 Agriculture/Ag Report")
agdat <- read.table('AgricultureWater.txt',header = T)

model <- lm(swc~poly(cwsi,2,raw=T),data=agdat,x = T)
df <- data.frame(cwsi = as.numeric(args[1]))
# df <- data.frame(cwsi = .4)
prediction <- predict(model,newdata = df,interval = 'confidence',level=0.95)
# print(args[1])
cat(' Prediction:',prediction[1],'\n',
    'Lower Bound:',prediction[2],'\n',
    'Upper Bound:',prediction[3],'\n')

# print(predict(model,newdata =df ))

# 24.666 + -10.597 * 0.4 + 6.067 * 0.4
# model <- lm(swc~poly(cwsi,2),data=agdat,x = T)
# rawmod <- lm(swc~poly(cwsi,2,raw=T),data=agdat,x = T)
# test <- cbind(1,.4,.4^2)
# bhat <- t(t(coef(model)))
# bhatraw <- t(t(coef(rawmod)))
# model$x[1:5,]
# rawmod$x[1:5,]
# test
# bhat
# 
# test %*% bhat
# test%*% bhatraw
# 
# val <- data.frame(cwsi = .4)
# predict(model,newdata = val )
# predict(rawmod,newdata = val )




