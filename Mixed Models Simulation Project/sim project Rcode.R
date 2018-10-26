#sim project code

# generate data under two different model types
# fit multiple models to the data and keep track of how often the IC
# chooses the best (true) model

###### SIMULATION CODE #############

### HOW TO FIT DIFFERENT MODEL TYPES

## compound symmetry ##
# library(lme4)
# fitcs <- lmer(response ~ -1 + as.factor(tmt) + ztime:as.factor(tmt) + (1|sub),data= weight)
# summary(fitcs)
# # the betas are about the same, but the standard errors change
# # quite a bit (whether bigger or smaller)
# 
# fitcs2 <- gls(response ~ -1 + as.factor(tmt) + ztime:as.factor(tmt),
#               correlation = corCompSymm(form=~1|sub),data= weight)
# summary(fitcs2)
# 
# fitcs2 <- gls(response ~ -1 + as.factor(tmt) + ztime:as.factor(tmt),
#                             correlation = corCompSymm(form=~1|sub),data= weight)
# 
# #data format
# # Generate a data set using the following general set-up and parameters. 
# # There are two treatment groups with five subjects per treatment. 
# # Each subject is measured at four equally spaced time periods. For group1,
# # the intercept is 10 and the slope is 1. For group 2, the intercept
# # is 10 and the slope is 1.5. 
# # Use an AR(1) covariance structure for the subjects with σ2 = 2.5 and ρ = .8
# # Then use CS structure with σ2e 2.5 and σ2b 1.5
# 
# #params
# sig2 <- 2.5
# rho <- 0.8
# g1b0 <- g2b0 <- 10
# g1b1 <- 1
# g2b1 <- 1.5
# #set up data set
# g1 <- rep(c(1,0),each=20)
# g2 <- rep(c(0,1),each=20)
# subj <- rep(1:10,each=4)
# subj
# time1 <- c(rep(0:3,5),rep(0,20))
# time2 <- c(rep(0,20),rep(c(0:3),5))
# time1
# X <- cbind(subj,g1,g2,time1,time2)
# X
# betas <- rbind(g1b0,g2b0,g1b1,g2b1)
# betas
# mean.vals <- X[,2:5] %*% betas
# mean.vals
# ## Set up the Covariance matrix for AR1
# N <- 4 
# R <- diag(N)
# R <- rho^(abs(row(R)-col(R))) ## AR(1) correlation matrix
# R#[1:5,1:5]
# m <- matrix(0,nrow=40,ncol=40)
# for(i in seq(1,37,by=4)){
#   m[i:(i+3),i:(i+3)] <- R
# }
# m
# vv <- sig2 * m #covariance matrix (AR1)
# y.ar1 <- mvrnorm(1,mu = mean.vals,Sigma = vv)
# 
# #for compound symmetric
# sig2 <- 2.5 #same overall variance as before as before
# rho <- 0.8
# v.cs <- matrix(rho,nrow=40,ncol=40) + diag(1-rho,40)
# v.cs

# R <- diag(sig2e,40)
# Z <- X[,2:3]
# G <- diag(4,2)
# v.cs <- Z %*% G %*% t(Z) + R
# v.cs

# y.cs <- mvrnorm(1,mu = mean.vals,Sigma = v.cs)
# 
# 
# 
# foo <- make_data(80)
# dim(foo)


#for AR1 (true model)






#setting up confidence intervals
# ci <- estimate + c(-1,1)*qnorm(1-0.05/2)*sqrt(estimate*(1-estimate)/nReps)

# estimate <- mean(test$AIC[,2]<test$AIC[,1])
# # estimate <- mean(test2$AIC[,2]<test2$AIC[,1])
# conf.bounds <- estimate + c(-1,1) *
#   qnorm(0.975) * sqrt(estimate*(1-estimate)/n.Iters)
# round(c(estimate,conf.bounds),3)
# 
# estimate2 <- mean(test2$AIC[,2]<test2$AIC[,1])
# conf.bounds <- estimate2 + c(-1,1) *
#   qnorm(0.975) * sqrt(estimate2*(1-estimate2)/n.Iters)
# round(c(estimate2,conf.bounds),3)

#########################################################


##############################################################################
### HELPER FUNCTIONS

#function to generate X matrix given a desired number of subjects
make_data <- function(size){
  
  #subject label
  subj <- rep(1:size,each=4)
  
  #split into two groups
  g1 <- rep(c(1,0),each=size * 2)
  g2 <- rep(c(0,1),each=size * 2)
  
  #make time measurement columns for each group
  time1 <- c(rep(0:3,size/2),rep(0,size*2))
  time2 <- c(rep(0,size*2),rep(c(0:3),size/2))
  
  #merge columns together
  X <- cbind(subj,g1,g2,time1,time2)
  # as.data.frame(X)
  X
}

MC.sim <- function(n.Iters = 20, X,sigma.matrix,mean.vals){
  # library(lme4)
  library(MASS)
  library(nlme)
  output <- NULL 
  AIC.vals <- matrix(0,nrow=n.Iters,ncol=2)
  BIC.vals <- matrix(0,nrow=n.Iters,ncol=2)
  for(i in 1:n.Iters){
    data <- X
    y <- mvrnorm(1,mu = mean.vals,Sigma = sigma.matrix)
    mod.data <- as.data.frame(cbind(data,y))
    #fit all models using gls
    
    # fit1 <-lmer(y ~ -1 + treatment + (1|block),REML=FALSE) # comp sym
    #use gls for other model types
    # maybe just do three different cov structures
    fitar1 <- gls(y ~ -1 + g1 + g2 + time1 + time2,
                  correlation = corAR1(form=~1|subj),data=mod.data)
    
    fitcs <- gls(y ~ -1 + g1 + g2 + time1 + time2,
                 correlation = corCompSymm(form=~1|subj),data=mod.data)
    AIC.vals[i,] <- c(AIC(fitar1),AIC(fitcs))
    BIC.vals[i,] <- c(BIC(fitar1),BIC(fitcs))
  }#end for loop
  
  output$AIC <- AIC.vals
  output$BIC <- BIC.vals
  output
}#end function

# test <- MC.sim(n.Iters = 10000,X = X,sigma.matrix = vv)
# test
# # how often is IC lower (better) for CS when true model is AR1
# mean(test$AIC[,2]<test$AIC[,1])
# mean(test$BIC[,2]<test$BIC[,1])
# test2 <- MC.sim(n.Iters = 10000,X = X,sigma.matrix = v.cs)
# test2
# # how often is IC lower (better) for AR1 when true model is CS
# mean(test2$AIC[,2]<test2$AIC[,1])
# mean(test2$BIC[,2]<test2$BIC[,1])


run_sim<- function(grid,nIters){
  
  #params
  sig2 <- 2.5
  # rho <- 0.8
  CS_list <- NULL
  AR1_list <- NULL
  for(i in 1:nrow(grid)){
    #get size and rho
    temp.size <- grid[i,2]
    temp.rho <- grid[i,1]
    
    #make X
    temp.X <- make_data(temp.size)
    temp.df <- as.data.frame(temp.X)
    
    #make variance matrix: AR1
    R <- diag(4)
    R <- temp.rho^(abs(row(R)-col(R)))
    m <- diag(1,temp.size) %x% R
    temp.v.ar1 <- sig2 * m
    
    #make variance matrix: AR1
    temp.v.cs <- matrix(temp.rho,nrow=4*temp.size,ncol=4*temp.size) +
      diag(1-temp.rho,4*temp.size)

    #make data (these are something that could be altered in further studies)
    g1b0 <- g2b0 <- 10
    g1b1 <- 1
    g2b1 <- 1.5
    betas <- rbind(g1b0,g2b0,g1b1,g2b1)
    temp.mean.vals <- temp.X[,2:5] %*% betas
    # y.ar1 <- mvrnorm(1,mu = mean.vals,Sigma = temp.v.ar1)
    # y.cs <- mvrnorm(1,mu = mean.vals,Sigma = temp.v.cs)
    
    trialname.cs <- paste(i,'cs',sep = '.')
    trialname.ar1 <- paste(i,'ar1',sep = '.')
    
    CS_list[[trialname.cs]] <- MC.sim(n.Iters = nIters,X = temp.df,sigma.matrix = temp.v.cs,
                                      mean.vals = temp.mean.vals)
    AR1_list[[trialname.ar1]] <- MC.sim(n.Iters = nIters,X = temp.df,sigma.matrix = temp.v.ar1,
                                        mean.vals = temp.mean.vals)
    
    
    
  }
  return(list(CS_list,AR1_list))
  
}

#input is foo[[1]]

##I think I need to fix this ## 
getCI <- function(n.Iters,sim.list){
  #get estimates and bounds
  est1 <- mean(sim.list$AIC[,2]<sim.list$AIC[,1])
  bounds1 <- est1 + c(-1,1) *
    qnorm(0.975) * sqrt(est1*(1-est1)/n.Iters)
  
  est2 <- mean(sim.list$BIC[,2]<sim.list$BIC[,1])
  bounds2 <- est2 + c(-1,1) *
    qnorm(0.975) * sqrt(est2*(1-est2)/n.Iters)
  #return values in matrix
  vals1 <- cbind(bounds1[1],est1,bounds1[2])
  vals2 <- cbind(bounds2[1],est2,bounds2[2])
  vals.all <- rbind(vals1,vals2)
  colnames(vals.all) <- c('Lower','Estimate','Upper')
  vals.all
}

#run all
rho.vals <- c(0.1,0.4,0.8)
size.vals <- c(10,40,80)
grid <- expand.grid(rho.vals,size.vals)
grid
iters <- 5

system.time(
foo <- run_sim(grid,iters)
# foo
)

system.time(
master <- run_sim(grid,nIters = 1000)
)
system.time(
MASTER <- run_sim(grid,nIters = 10000)
)


####double for loop to get all estimates and intervals
#get values
list.of.estimates <- list()
final <- master
#loops through cs as true model and ar1 as true model:
for(i in 1:2){
  
  for(j in 1:length(final[[i]])){
    temp.list <- final[[i]][[j]]
    ind <- 9 * (i-1) + j
    list.of.estimates[[ind]] <- getCI(n.Iters = 1000,sim.list = temp.list)
  }
}
list.of.estimates


#make tables from list of elements for paper
library(xtable)
#compund symmetric generated data results:

### poorly named
a <- sapply(list.of.estimates,'[',c(1,3,5))
CS.all <- matrix(a,nrow=18,ncol=3,byrow=T)
CS.all

xtable(cbind(grid,CS.all[1:9,]))#AIC CS
xtable(cbind(grid,CS.all[10:18,]))#AIC AR1

#
b <- sapply(list.of.estimates,'[',c(2,4,6))
bic.all <- matrix(a,nrow=18,ncol=3,byrow=T)
bic.all

xtable(cbind(grid,bic.all[1:9,]))#BIC CS
xtable(cbind(grid,bic.all[10:18,]))#BIC AR1








###########################
####### SCRATCH CODE #####
#setting up confidence intervals
# ci <- estimate + c(-1,1)*qnorm(1-0.05/2)*sqrt(estimate*(1-estimate)/nReps)

estimate <- mean(test$AIC[,2]<test$AIC[,1])
# estimate <- mean(test2$AIC[,2]<test2$AIC[,1])
conf.bounds <- estimate + c(-1,1) *
  qnorm(0.975) * sqrt(estimate*(1-estimate)/n.Iters)
round(c(estimate,conf.bounds),3)

estimate2 <- mean(test2$AIC[,2]<test2$AIC[,1])
conf.bounds <- estimate2 + c(-1,1) *
  qnorm(0.975) * sqrt(estimate2*(1-estimate2)/n.Iters)
round(c(estimate2,conf.bounds),3)



foo[[1]][[1]][[1]]
foo[[1]][[1]]

blah <- foo[[1]][[1]]
blah










