### Missing Data Report Code ###

### IMPUTATION FUNCTION ###
conditional_sampling <- function(x,n.draws,n.burn,mu.init=NULL,sig.init=NULL){
  library(mvtnorm) #needed to generate MVN draws
  # PARAMETERS --------------------------------------------------
  # x = the matrix of data (not a dataframe)
  # n.draws = number of draws to use for param estimation
  # n.burn = number of values to throw out before estimating
  # mu.init = intial value for mu (vector)
  # sig.init = intial values of sigma (matrix) 
  # -------------------------------------------------------------
  
  #set intial values:
  #if no initial values is specified use sample mean/variance of complete cases
  if(is.null(mu.init)){
    mean.vec <- as.matrix(apply(na.exclude(x),2,mean))
  }else{
    mean.vec <- mu.init
  }
  if(is.null(sig.init)){
    # sig <- as.matrix(var(na.exclude(x))) #method 1: only complete cases
    sig <- cov(x,use = 'pairwise.complete.obs') #method 2: pairwise complete cases
  }else{
    sig <- sig.init
  }
  
  #set up for the imputation algorithm:
  output <- NULL #list of all things returned at end of function
  n.iters <- n.draws + n.burn
  p <- dim(x)[2]
  # vvv this is where draws will be stored vvv
  mean.draws <- matrix(0,nrow=n.iters,ncol=p)
  sig.draws <- array(0,dim=c (p,p,n.iters))
  
  #store model coefs for each iteration
  model.coefs <- matrix(0,nrow = n.iters,ncol=p)
  var.within <- matrix(0,nrow = n.iters,ncol=p)
  r.sq <- rep(NA,n.iters)
  # var.between <- matrix(0,nrow = n.iters,ncol=p)
  
  missvals <- is.na(x) #matrix (not vector) of logicals (TRUE => NA)
  new.impute <- x #imputed values stored for each iteration
  count.iters <- 1
  while(count.iters <= n.iters){
    for(i in 1:nrow(x)){
      #check if any values in the ith row are missing
      pick.miss <-c(missvals[i,]) #vector of logicals: TRUE => NA(missing)
      
      #if any values in the row are missing, then:
      if (sum(pick.miss) != 0){
        # Calculate mean vector and sigma estimates,
        # use those to get random draw for values
        
        # FORMULAS ARE BASED ON CONDITION MULTIVARIATE NORMAL DISTRIBUTION
        # calculate mean vector
        temp.means <- mean.vec[pick.miss] + #column means for NA vals in ith row
          sig[pick.miss,!pick.miss] %*% #covariance (missing x non-missing)
          solve(sig[!pick.miss,!pick.miss]) %*% #inv.cov (non-missing x non-missing)
          (t(t(new.impute[i,!pick.miss]))- t(t(mean.vec[!pick.miss])))
        #calculate variance covariance matrix
        temp.variances <- 
          sig[pick.miss,pick.miss] - #cov(missing|missing)
          sig[pick.miss,!pick.miss] %*% #cov (missing|non-missing)
          solve(sig[!pick.miss,!pick.miss]) %*% #inv.cov(non-missing|non-missing)
          sig[!pick.miss,pick.miss] #cov.(non-missing|missing)
        
        #fill missing values with a random draw of MVN(est.mean,est.variance)
        new.impute[i,pick.miss] <-
          rmvnorm(1,mean=temp.means,sigma = temp.variances)
      }
      #else if none of the values are missing then do nothing
    }#end of for loop
    
    #after all NA values have been replaced, update mean.vec and sig
    sig <- var((new.impute))
    mean.vec <- as.matrix(apply(new.impute,2,mean))
    
    #fit the model and extract coefs and variation terms
    fit <- lm(JobPerf~.,data = as.data.frame(new.impute))
    r.sq[count.iters] <- summary(fit)$r.squared
    fit.sum <- summary(fit)$coefficients
    model.coefs[count.iters,] <- fit.sum[,1] #each coef estimate
    var.within[count.iters,] <- fit.sum[,2]^2 #the squared SE of each coefficient estimate

    #then add the draw to the matrix of draws?
    mean.draws[count.iters,] <- mean.vec
    
    sig.draws[,,count.iters] <- sig
    
    cat(count.iters,'\n') #for debugging
    count.iters <- count.iters+1 #increment for while loop
  }#end while loop
  
  #return all draws except burned draws
  output$MeanDraws <- mean.draws[-c(1:n.burn),] 
  output$SigmaDraws <- sig.draws[,,-c(1:n.burn)]
  output$Model.Coefs <- model.coefs[-c(1:n.burn),]
  output$Var.List <- var.within[-c(1:n.burn),]
  output$R.Squared.Draws <- r.sq[-c(1:n.burn)]
  #return estimates of parameters
  #take mean of mean.draws to estimate mean
  output$MeanEst <- apply(output$MeanDraws,2,mean)
  #take the mean of the covariance draws
  output$CovarianceEst <- apply(output$SigmaDraws,c(1,2),mean)
  output$Coef.Means <- apply(output$Model.Coefs,2,mean)
  output$Var.Within <- apply(output$Var.List,2,mean)
  output$Var.Between <- apply(output$Model.Coefs,2,var)
  output$R.Squared.Mean <- mean(output$R.Squared.Draws)
  #return a list of the draws and estimates
  return(output)
}#end function

summary(fit.i)$r.squared

###### ANALYSIS ##########
#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 2 MissingData/MissingDataReport")
employee <- read.table('employee.txt',header=T)
employee <- employee[,names(employee) != 'ID']#drop ID column
employee <- employee[,c(1:4,6,5)] #move JobPerf to the last column
head(employee)#sanity check

##employee data summary
#note that only well being, job sat, and job performance have NA's
summary(employee)

#check model assumptions on complete cases only
fit.diag <- lm(JobPerf~.,data = employee[complete.cases(employee),])
library(car)
pdf('avplots.pdf')
avPlots(fit.diag)
dev.off()

pdf('residplot.pdf')
plot(fit.diag$fitted.values,fit.diag$residuals,
     xlab='Fitted Values',ylab='Residuals',
     main='Fitted Values vs. Residuals')
abline(h=0,col='red')
dev.off()

pdf('residhist.pdf')
hist(fit.diag$residuals,prob=T, main='Histogram of Residuals',
     xlab='',ylim=c(0,.35))
curve(dnorm(x,0,sd(fit.diag$residuals)),col='red',lwd=2,add=T)
dev.off()


##IMPUTE DATA
test <- conditional_sampling(as.matrix(employee),n.draws = 200,n.burn = 0)

test$R.Squared.Mean

## Generate Trace Plots
#note that for variables with no missing values trace plots are flat (as expected)
plot(test$MeanDraws[,1],type='l')
plot(test$MeanDraws[,2],type='l')
plot(test$MeanDraws[,3],type='l')
plot(test$MeanDraws[,4],type='l', ylab = '',xlab='Iteration',
     main = 'Well Being Estimates')  #well being
plot(test$MeanDraws[,5],type='l', ylab = '',xlab='Iteration',
     main = 'Job Satisfaction') #job satisfaction
plot(test$MeanDraws[,6],type='l', ylab = '',xlab='Iteration',
     main = 'Job Performance') #job performance
plot(test$MeanDraws[,7],type='l')


#this takes about 12 minutes to run, with 5000 iterations and 500 burns
M <- 5000
num.burn <- 500
system.time(
impute.sims <- conditional_sampling(as.matrix(employee),n.draws = M,n.burn = num.burn)
)

###RESULTS
names(impute.sims)

##Mean of Coef estimates
impute.sims$Coef.Means
impute.sims$R.Squared.Mean

##Confidence Intervals
#pooled variance
var.together <- impute.sims$Var.Within + impute.sims$Var.Between + 
  impute.sims$Var.Between/M
#FMI
FMI <- (impute.sims$Var.Between + impute.sims$Var.Between/M)/var.together
FMI

nu <- (M-1) * (1/FMI^2)
nu

#significance testing
pvals <- 1-pt(impute.sims$Coef.Means/sqrt(var.together),nu)

ci.upper <- impute.sims$Coef.Means + qt(.975,nu) * sqrt(var.together)
ci.lower <- impute.sims$Coef.Means - qt(.975,nu) * sqrt(var.together)
ci <- cbind(Estimate = impute.sims$Coef.Means,
            '2.5%' = ci.lower,
            '95%' = ci.upper,
            'P-value' = pvals)
row.names(ci) <- c("Intercept","Age","Tenure","WellBeing","JobSat","IQ")
ci

# #sanity check 
# #look at estimates/intervals for 1 iteration
# fit1 <- lm(JobPerf~.,data=as.data.frame(new.impute))
# confint(fit1)
# #complete cases only
# fit2 <- lm(JobPerf~.,data=employee[complete.cases(employee),])
# fit2$coefficients
# confint(fit2)


### PLOTS AND TABLES
#Trace Plots
pdf('TracePlots.pdf')
par(mfrow=c(3,1))
plot(impute.sims$MeanDraws[,3],type='l', ylab = '',xlab='Iteration',
     main = 'Well Being')  #well being
plot(impute.sims$MeanDraws[,4],type='l', ylab = '',xlab='Iteration',
     main = 'Job Satisfaction') #job satisfaction
plot(impute.sims$MeanDraws[,6],type='l', ylab = '',xlab='Iteration',
     main = 'Job Performance') #job performance
dev.off()

#Confidence Interval Table
library(xtable)
xtable(ci)


#R squared
hist(impute.sims$R.Squared.Draws)

impute.sims$R.Squared.Mean
quantile(impute.sims$R.Squared.Draws,c(0.025,.975))











