
#Code credit goes to author of this blog post:
# https://www.r-bloggers.com/imputing-missing-data-with-expectation-maximization/


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

##### RUN ALGORITHM ###############
library(mvtnorm) #for random generating
library(norm) #for em.norm
library(mvnmle)
n <- 100
true.means <- c(0,10,20,40,50)
true.covariances <- matrix(c(3,0,0,0,0,
                             0,3,0,0,0,
                             0,0,3,0,0,
                             0,0,0,3,0,
                             0,0,0,0,3),byrow=T,ncol=5,nrow=5)
# true.covariances
test <- rmvnorm(n,mean=true.means,sigma = true.covariances)
test.orig <- test
#randomly delete some of the data
miss.amount <- 0.3 #percentage of data to remove
rand.miss <- sample(c(1:length(test)),length(test) %/% (1/miss.amount))
test[rand.miss] <- NA
sum(is.na(test))
df.test <- data.frame(test)
df.test

#this will throw an error if there is an entire row of missing values
conv.vals <- EMalg(df.test, tol=.001)

#compare means
conv.vals$MeanEst
true.means
#new data
conv.vals$ImputedData
conv.vals$MeanIters



## PLOTTING CONVERGENCE ##

#### PLOT the convergence ####

library(reshape2)
library(ggplot2)

#wonky initial values
#  wonky init values = c(-10,50,110,0,25); True means: 0 10 20 40 50
wonky.iters <- rbind(c(-10,50,80,0,25),conv.vals$MeanIters[1:38,])
wonky.iters

# write.csv(wonky.iters,'wonkyiters.csv',row.names = F)
# read.csv('wonkyiters.csv')
m1 <- melt(wonky.iters)#melt(c.samps$MeanDraws)
head(m1)
names(m1) <- c("Iteration","Group","Estimate")
m1$Group <- as.factor(m1$Group)

#save convergence graph for wonky
png('EMconvergence-wonky.png')
my_note <- 'True Means: 0, 10, 20, 40, 50\nEstimates: 0.24, 10.28, 19.62, 39.72, 49.97'
ggplot(data=m1, aes(x=Iteration, y=Estimate, group=Group, colour=Group)) +
  geom_line() + #makes into a line plot
  geom_point() + #adds points
  ggtitle("EM Algorithm Convergence") +
  annotate("text", x = 24, y = 65, label = my_note)
dev.off()


my_note_wonky <- 'True Means: 0, 10, 20, 40, 50\nEstimates: 0.24, 10.28, 19.62, 39.72, 49.97'



#good start values:
good.iters <- rbind(apply(na.exclude(df.test),2,mean),conv.vals$MeanIters[1:20,])
colnames(good.iters) <- c(1,2,3,4,5)
good.iters
# write.csv(good.iters,'gooditers.csv',row.names = F)

m2 <- melt(good.iters)
names(m1) <- c("Iteration","Group","Estimate")
m2$Group <- as.factor(m2$Group)

#save convergence graph for good vals
png('EMconvergence-goodstart.png')
my_note <- 'True Means: 0, 10, 20, 40, 50\nEstimates: 0.24, 10.28, 19.62, 39.72, 49.97'
ggplot(data=m2, aes(x=Iteration, y=Estimate, group=Group, colour=Group)) +
  geom_line() + #makes into a line plot
  geom_point() + #adds points
  ggtitle("EM Algorithm Convergence") +
  annotate("text", x = 12, y = 30, label = my_note)
dev.off()





