########################
### Matt Oehler ########
### STAT 624 Project 2 #
########################

###conditional sampling method

#LIBRARIES
# install.packages('mvtnorm')
library(mvtnorm) #for random generating
library(reshape2)
###POSSIBLE COVARIANCE MATRICIES
#led to warnings
matrix(c(1,-.4,.6,.7,-.3,
         -.4,1,.1,.8,.2,
         .6,.1,1,.3,.4,
         .7,.8,.3,1,-.2,
         -.3,.2,.4,-.2,1),byrow=T,ncol=5,nrow=5)
#no warnings
matrix(c(1,.4,.6,.7,.3,
         .4,1,.1,.8,.2,
         .6,.1,1,.3,.4,
         .7,.8,.3,1,.2,
         .3,.2,.4,.2,1),byrow=T,ncol=5,nrow=5)


#generate random MVN data set
n <- 500
true.means <- c(0,1,2,4,5)
true.covariances <- matrix(c(1,.4,.6,.7,.3,
                             .4,1,.1,.8,.2,
                             .6,.1,1,.3,.4,
                             .7,.8,.3,1,.2,
                             .3,.2,.4,.2,1),byrow=T,ncol=5,nrow=5)
true.covariances                       
test <- rmvnorm(n,mean=c(0,10,20,40,50),sigma = true.covariances)
test.orig <- test
#randomly delete some of the data
miss.amount <- 0.3 #percentage of data to remove
rand.miss <- sample(c(1:length(test)),length(test) %/% (1/miss.amount))
test[rand.miss] <- NA
sum(is.na(test))#number of missing values

### CONDITIONAL SAMPLER ###

#Uses Conditional Normal Theory ###
#this won't converge, rather we will be getting draws from the distributions of the parameters
#and we will estimate the params based on the draws

test #test dataset

n.draws <- 10000 #draws
n.burn <- 1000 #burn-in

conditional_sampling <- function(x,n.draws,n.burn,mu.init=NULL,sig.init=NULL){
  library(mvtnorm) #needed to generate MVN draws
  # PARAMETERS --------------------------------------------------
  # x = the matrix of data (missing values == NA)
  # n.draws = number of draws to use for param estimation
  # n.burn = number of values to throw out before estimating
  # mu.init = intial value for mu (vector)
  # sig.init = intial values of sigma (matrix) 
  # -------------------------------------------------------------
  #(unless otherwise specified):
  #set initial values as sample mean/variance of data after removing NA values
  if(is.null(mu.init)){
    mean.vec <- as.matrix(apply(na.exclude(x),2,mean))
  }else{
    mean.vec <- mu.init
  }
  if(is.null(sig.init)){
    sig <- as.matrix(var(na.exclude(x)))
  }else{
    sig <- sig.init
  }
  
  output <- NULL 
  n.iters <- n.draws + n.burn
  p <- dim(x)[2]
  mean.draws <- matrix(0,nrow=n.iters,ncol=p)#this is where draws will be stored
  sig.draws <- array(0,dim=c(p,p,n.iters))
  
  missvals <- is.na(x) #matrix (not vector) of logicals (TRUE => NA)
  new.impute <- x #imputed values will be stored to this 
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
    
    #then add the draw to the matrix of draws?
    mean.draws[count.iters,] <- mean.vec
    
    sig.draws[,,count.iters] <- sig
    
    count.iters <- count.iters+1 #increment for while loop
    cat(count.iters,'\n') #for debugging
  }#end while loop
  
  #return all draws except burned draws
  output$MeanDraws <- mean.draws[-c(1:n.burn),] 
  output$SigmaDraws <- sig.draws[,,-c(1:n.burn)]
  
  #return estimates of parameters
  #take mean of mean.draws to estimate mean
  output$MeanEst <- apply(output$MeanDraws,2,mean)
  #take the mean of the covariance draws
  output$CovarianceEst <- apply(output$SigmaDraws,c(1,2),mean)
  
  
  
  #return a list of the draws and estimates
  return(output)
}#end function

#takes a few seconds
c.samps <- conditional_sampling(test,n.draws=1000,n.burn=100)

#compare values
true.means - c.samps$MeanEst
true.covariances - c.samps$CovarianceEst


#plot of means
library(ggplot2)
library(reshape2)

m1 <- melt(c.samps$MeanDraws)
head(m1)
ggplot(m1, aes(x=Var1, y=value, col=Var2))+
  geom_line()


