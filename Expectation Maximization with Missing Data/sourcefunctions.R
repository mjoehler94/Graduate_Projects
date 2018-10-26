#functions to be sourced

#### EM ALGORITHM
#function that runs EM algorithm
#note: x must be a data.frame
EMalg <- function(x, tol=.001){
  
  #for debugging
  # all.imputes <- array(0,dim=c(dim(x)[1],dim(x)[2],100))
  
  out <- NULL
  p <- dim(x)[2]
  
  
  missvals <- is.na(x) #matrix (not vector) of logicals (TRUE => NA)
  new.impute <- x #new&old will later be compared to check tolerance level
  # old.impute <- x
  count.iter <- 1 #for debugging
  reach.tol <- 0 #indicator for while loop (becomes 1 when tolerance is reached)
  
  #set starting values as sample mean/var of data after removing missing values
  sig <- as.matrix(var(na.exclude(x)))
  mean.vec <- as.matrix(apply(na.exclude(x),2,mean)) #original
  # VVV DON'T USE THIS AS STARTING VALUE!!!! VVV
  # mean.vec <- matrix(c(-10,50,80,0,25),nrow=5,ncol=1)
  
  mean.iters <- matrix(mean.vec,nrow=1,ncol=p) #stores estimates from each iteration
  #loop until convergence
  while(reach.tol != 1) {
    for(i in 1:nrow(x)) {
      # cat(i,'\n')
      #check if any values in the ith row are missing
      pick.miss <-( c( missvals[i,]) ) #vectorL10 of logicals: TRUE => NA(missing)
      
      #if any values in the row are missing, then:
      if ( sum(pick.miss) != 0 ) {
        # we take the inverse of the sample cov matrix without the rows/cols 
        # that correspond to the missing value(s)
        inv.S <- solve(sig[!pick.miss,!pick.miss]) 
        
        # Run the EM algorithm:
        # set missing value(s) in ith row = column mean + 
        # (cov.matrix of )
        new.impute[i,pick.miss] <- mean.vec[pick.miss] +
          sig[pick.miss,!pick.miss] %*%
          inv.S %*%
          (t(new.impute[i,!pick.miss])- t(t(mean.vec[!pick.miss])))
      }
      #else if none of the values are missing then do nothing
    }#end of for loop
    
    # all.imputes[,,count.iter] <- as.matrix(new.impute)
    
    #after all values have been replaced during the loop
    sig <- var((new.impute))
    mean.vec <- as.matrix(apply(new.impute,2,mean))
    
    mean.iters <-rbind(mean.iters, t(mean.vec))
    # cat(count.iter,'\n') #for debugging
    #DEFAULT CONVERGENCE CHECK DOESN'T WORK IF LAST VALUE ISN'T MISSING
    # we don't want this to run on the first iteration or else it fails
    # if(count.iter > 1){
    #   for(l in 1:nrow(new.impute)){
    #     for(m in 1:ncol(new.impute)){
    #       if( abs((old.impute[l,m]-new.impute[l,m])) > tol ) {
    #         reach.tol <- 0 #tolerance not reached => continue looping
    #       } else {
    #         reach.tol <- 1#tolerance reached => break out of while loop
    #       }
    #     }
    #   }
    # }
    
    # updated convergence check
    if(count.iter > 1){
      temp.diff <- abs(old.impute-new.impute)
      # if any of the values in the temp.matrix exceed threshold,
      # then we haven't converged yet
      temp.check  <- temp.diff > tol #matrix of logicals (TRUE => not converged yet)
      # if(sum(old.impute - new.impute) == 0) cat('new == old') # for debugging
      if(sum(temp.check) < 1){
        reach.tol <- 1
      }
    }
    
    count.iter <- count.iter+1 #for debugging: to ensure process is iterating properly
    
    old.impute <- new.impute #update old impute
  }#end while loop
  out$ImputedData <- new.impute
  out$MeanIters <- mean.iters
  out$MeanEst <- mean.vec
  # out$imputes <- all.imputes #for debugging
  return(out)
}#end EMalg function




#### CONDITIONAL SAMPLER

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
  sig.draws <- array(0,dim=c (p,p,n.iters))
  
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
    
    # VVV ??????????????? VVV
    #then add the draw to the matrix of draws?
    mean.draws[count.iters,] <- mean.vec
    
    sig.draws[,,count.iters] <- sig
    
    count.iters <- count.iters+1 #increment for while loop
    # cat(count.iters,'\n') #for debugging
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

