# Matt Oehler Stat 624
# project2 MC simulations

# SOURCED FILES
# source('conditional_sampler.R')

# LIBRARIES
# library(mvtnorm) #for random generating
# library(norm) #for em.norm
# library(mvnmle) #for mle of mvn

#### FUNCTIONS #####################################

##simulation for throw away method
#data removal methods: 1 => random removal; 2 => remove high values
mc_throw_away <- 
  function(nReps=10000,samp.size=100,true.mean,true.covariance,
           miss.amount=0.3,method = 1){
  # PARAMETERS --------------------------------------------------
  # true.mean = a vector of means
  # true.sig = a covariance matrix that corresponds to true.mean
  # miss.amount = amount of data to remove
  # method = how we remove the data random or not
  # -------------------------------------------------------------
  library(mvtnorm) #for random data generation
  out <- NULL
  p <- length(true.mean)#dimension
  mean.draws <- matrix(0,nrow=nReps,ncol=p)
  sig.draws <- array(0,dim=c(p,p,nReps))
  
  for(i in 1:nReps){
    if (i %% 500==0) {cat(i,'\n')}
    #simulate data
    vals <- rmvnorm(samp.size,mean=true.mean,sigma = true.covariance)
    if(method==1){#randomly remove values from data
      # miss.amount <- 0.3 #percentage of data to remove
      rand.miss <- sample(c(1:length(vals)),length(vals) %/% (1/miss.amount))
      vals[rand.miss] <- NA
    }else if(method == 2){
      #remove values that exceed 1.5 standard dev. above the mean
      threshold <- apply(vals,2,sd)
      for(j in 1:p){
        rem.vals <- which(vals[,j] > 1.5*threshold[j]+true.mean[j])
        vals[rem.vals,j] <- NA
      }
    }else{
      cat('invalid method\n');return(NULL)
    }
    
    #estimate params with throw away method and store draws
    #remove any rows that are all NA 
    vals <- vals[rowSums(is.na(vals)) != ncol(vals),]
    # cat('vals removed:',sum(is.na(vals)),'\n')
    
    no.miss <- vals[complete.cases(vals),] #dataset after removing missing vals
    
    #Get maximum likelihood estimates (after removing missing values)
    # mean.draws[i,] <- mlest(vals[complete.cases(vals),])$muhat #mean vector
    # sig.draws[,,i] <- mlest(vals[complete.cases(vals),])$sigma #matrix
    mean.draws[i,] <- apply(no.miss,2,mean) #mean vector
    sig.draws[,,i] <- var(no.miss) #matrix
    
  }
  #store draws, calculate bias, and MSE, and overall estimate
  out$MeanDraws <- mean.draws
  out$CovarianceDraws <- sig.draws
  #bias
  mean.bias.matrix <- t(t(mean.draws) - true.mean) #creates matrix of biases
  sigma.bias.array <- sig.draws - array(true.covariance,dim=c(p,p,nReps))
  out$bias<-list(mu.bias=apply(mean.bias.matrix,2,mean),
                 sigma.bias= apply(sigma.bias.array,c(1,2),mean))
  #MSE
  out$MSE<-list(mu.mse=apply(mean.bias.matrix^2,2,mean),
                sigma.mse=apply(sigma.bias.array^2,c(1,2),mean))
  #MLE
  out$MLE.est <- list(mu.hat=apply(mean.draws,2,mean),
                      sigma.hat=apply(sig.draws,c(1,2),mean))
  return(out)
}

## EM SIMULATIONS 
mc_em_alg <- 
  function(nReps=10000,samp.size=100,true.mean,true.covariance,
                      miss.amount = 0.3,method=1,tol=0.001){
  #add libraries and source functions from other files
  source('sourcefunctions.R')
  library(mvtnorm) #for random data generation
  out <- NULL
  p <- length(true.mean)#dimension
  mean.draws <- matrix(0,nrow=nReps,ncol=p)
  sig.draws <- array(0,dim=c(p,p,nReps))
  
  for(i in 1:nReps){
    if (i %% 500==0) {cat(i,'\n')}
    #simulate data
    vals <- rmvnorm(samp.size,mean=true.mean,sigma = true.covariance)
    if(method==1){#randomly remove values from data
      # miss.amount <- 0.3 #percentage of data to remove
      rand.miss <- sample(c(1:length(vals)),length(vals) %/% (1/miss.amount))
      vals[rand.miss] <- NA
    }else if(method == 2){
      #remove values that exceed 1.5 standard dev. above the mean
      threshold <- apply(vals,2,sd)
      for(j in 1:p){
        rem.vals <- which(vals[,j] > 1.5*threshold[j]+true.mean[j])
        vals[rem.vals,j] <- NA
      }
    }else{
      cat('invalid method\n');return(NULL)
    }
    
    #remove any rows that are all NA 
    vals <- vals[rowSums(is.na(vals)) != ncol(vals),]
    
    run.alg <- EMalg(as.data.frame(vals),tol)
    mean.draws[i,] <- t(run.alg$MeanEst)
    sig.draws[,,i] <-  var(run.alg$ImputedData)
  }#end for loop
  
  #store draws, calculate bias, and MSE, and overall estimate
  out$MeanDraws <- mean.draws
  out$CovarianceDraws <- sig.draws
  #bias
  mean.bias.matrix <- t(t(mean.draws) - true.mean) #creates matrix of biases
  sigma.bias.array <- sig.draws - array(true.covariance,dim=c(p,p,nReps))
  out$bias<-list(mu.bias=apply(mean.bias.matrix,2,mean),
                 sigma.bias= apply(sigma.bias.array,c(1,2),mean))
  #MSE
  out$MSE<-list(mu.mse=apply(mean.bias.matrix^2,2,mean),
                sigma.mse=apply(sigma.bias.array^2,c(1,2),mean))
  #MLE
  out$MLE.est <- list(mu.hat=apply(mean.draws,2,mean),
                      sigma.hat=apply(sig.draws,c(1,2),mean))
    
  return(out)
}#end of function


## Conditional Sampling
mc_conditional <- 
  function(nReps=10000,samp.size=100,true.mean,true.covariance,
           miss.amount = 0.3,method=1,tol=0.001){
    #add libraries and source functions from other files
    source('sourcefunctions.R')
    library(mvtnorm) #for random data generation
    out <- NULL
    p <- length(true.mean)#dimension
    mean.draws <- matrix(0,nrow=nReps,ncol=p)
    sig.draws <- array(0,dim=c(p,p,nReps))
    
    for(i in 1:nReps){
      if (i %% 500==0) {cat(i,'\n')}
      #simulate data
      vals <- rmvnorm(samp.size,mean=true.mean,sigma = true.covariance)
      if(method==1){#randomly remove values from data
        # miss.amount <- 0.3 #percentage of data to remove
        rand.miss <- sample(c(1:length(vals)),length(vals) %/% (1/miss.amount))
        vals[rand.miss] <- NA
      }else if(method == 2){
        #remove values that exceed 1.5 standard dev. above the mean
        threshold <- apply(vals,2,sd)
        for(j in 1:p){
          rem.vals <- which(vals[,j] > 1.5*threshold[j]+true.mean[j])
          vals[rem.vals,j] <- NA
        }
      }else{
        cat('invalid method\n');return(NULL)
      }
      
      #remove any rows that are all NA 
      vals <- vals[rowSums(is.na(vals)) != ncol(vals),]
      
      # run.alg <- EMalg(as.data.frame(vals),tol)
      run.samp <- conditional_sampling(vals,n.draws=25,n.burn=5)
      
      mean.draws[i,] <- run.samp$MeanEst
      sig.draws[,,i] <-  run.samp$CovarianceEst
    }#end for loop
    
    #store draws, calculate bias, and MSE, and overall estimate
    out$MeanDraws <- mean.draws
    out$CovarianceDraws <- sig.draws
    #bias
    mean.bias.matrix <- t(t(mean.draws) - true.mean) #creates matrix of biases
    sigma.bias.array <- sig.draws - array(true.covariance,dim=c(p,p,nReps))
    out$bias<-list(mu.bias=apply(mean.bias.matrix,2,mean),
                   sigma.bias= apply(sigma.bias.array,c(1,2),mean))
    #MSE
    out$MSE<-list(mu.mse=apply(mean.bias.matrix^2,2,mean),
                  sigma.mse=apply(sigma.bias.array^2,c(1,2),mean))
    #MLE
    out$MLE.est <- list(mu.hat=apply(mean.draws,2,mean),
                        sigma.hat=apply(sig.draws,c(1,2),mean))
    
    return(out)
  }#end of function

### END OF FUNCTIONS ###


#### RUN SIMULATIONS ##########################
#values
mu <- c(0,10,20,40,50)
sigma <- matrix(c(1,.4,.6,.7,.3,
                  .4,1,.1,.8,.2,
                  .6,.1,1,.3,.4,
                  .7,.8,.3,1,.2,
                  .3,.2,.4,.2,1),byrow=T,ncol=5,nrow=5)
### THROW AWAY METHOD
#run throw away simulation (random missing data)

#should take 4ish  minutes at nReps == 10000
system.time(
run1 <- mc_throw_away(10000,samp.size = 100,
                      true.mean = mu,true.covariance = sigma,
                      method=1))
run1$bias
run1$MSE
run1$MLE.est
# confints1 <- run1$MLE.est$mu.hat + c(-1,1) 

#high vals only are removed
system.time(
run2 <- mc_throw_away(10000,samp.size = 100,
                    true.mean = mu,true.covariance = sigma,
                    method=2)
)
run2$bias
run2$MSE
run2$MLE.est

### EXPECTATION MAXIMIZATION
#use values above

system.time(
em_run1 <- mc_em_alg(nReps=1000,samp.size=100,
                     true.mean=mu,true.covariance=sigma,
                     method=1,miss.amount=0.3,tol=0.001)

)
em_run1$bias
em_run1$MSE
em_run1$MLE.est

#using method 2 and removing upper values
system.time(
  em_run2 <- mc_em_alg(nReps=10,samp.size=100,
                       true.mean=mu,true.covariance=sigma,
                       method=2,miss.amount=0.3,tol=0.001)
  
)

em_run2$bias
em_run2$MSE
em_run2$MLE.est

#### Run conditional sampler ###
#use values above
#takes 1.5 minutes per 100... :(
system.time(
  conditional_run1 <- mc_conditional(nReps=1000,samp.size=100,
                       true.mean=mu,true.covariance=sigma,
                       method=1,miss.amount=0.3,tol=0.001)
  
)
conditional_run1$bias
conditional_run1$MSE
conditional_run1$MLE.est

#using method 2 and removing upper values
#takes about a minute per 1000 iters
system.time(
  conditional_run2 <- mc_em_alg(nReps=10000,samp.size=100,
                       true.mean=mu,true.covariance=sigma,
                       method=2,miss.amount=0.3,tol=0.001)
  
)

conditional_run2$bias
conditional_run2$MSE
conditional_run2$MLE.est


###xtable code
namevec1 <- c('Throw Away','EM Algorithm','Conditional Sampling')#rows
namevec2 <- c('Estimate','Bias','MSE')#cols
all.means1 <- c(run1$MLE.est$mu.hat,em_run1$MLE.est$mu.hat,conditional_run1$MLE.est$mu.hat)
all.means2 <- c(run2$MLE.est$mu.hat,em_run2$MLE.est$mu.hat,conditional_run2$MLE.est$mu.hat)

all.bias1 <- c(run1$bias$mu.bias,em_run1$bias$mu.bias,conditional_run1$bias$mu.bias)
all.bias2 <- c(run2$bias$mu.bias,em_run2$bias$mu.bias,conditional_run2$bias$mu.bias)

all.mse1 <- c(run1$MSE$mu.mse,em_run1$MSE$mu.mse,conditional_run1$MSE$mu.mse)
all.mse2 <- c(run2$MSE$mu.mse,em_run2$MSE$mu.mse,conditional_run2$MSE$mu.mse)

tab1 <- round(cbind(all.means1,all.bias1,all.mse1),4) 
colnames(tab1) <- namevec2
tab1

all.ta1 <- c(run1$MLE.est$mu.hat,run1$bias$mu.bias,run1$MSE$mu.mse)
all.em1 <- c(em_run1$MLE.est$mu.hat,em_run1$bias$mu.bias,em_run1$MSE$mu.mse)
all.cs1 <- c(conditional_run1$MLE.est$mu.hat,conditional_run1$bias$mu.bias,conditional_run1$MSE$mu.mse)

round(cbind(all.ta1,all.em1,all.cs1),2)

#make tables
tab1 <- round(cbind(all.means1,all.bias1,all.mse1),4) 
colnames(tab1) <- namevec2
tab1

tab2 <- cbind(all.means2,all.bias2,all.mse2)
colnames(tab2) <- namevec2
tab2
print(xtable(tab2,digits=4))








