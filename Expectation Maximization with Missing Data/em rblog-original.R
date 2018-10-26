#Code credit goes to author of this blog post:
# https://www.r-bloggers.com/imputing-missing-data-with-expectation-maximization/

# install.packages('e1071')
library(e1071) #for rdiscrete


#fake data called straw
# straw <- replicate(10, rpois(50,100))
# straw.orig <- straw
true.mu <- c(10,30,50,4,90)
a <- rpois(50,true.mu[1])
b <- rpois(50,true.mu[2])
c <- rpois(50,true.mu[3])
d <- rpois(50,true.mu[4])
e <- rpois(50,true.mu[5])
straw <- cbind(a,b,c,d,e)

rand.miss <- rdiscrete(50,probs=rep(1:length(straw)), values=seq(1,length(straw)) )
straw[rand.miss] <- NA

straw <- data.frame(straw)
straw
var(na.omit(straw) )
# var(raw.imputed)

EMalg.orig <- function(x, tol=.001){
  missvals <- is.na(x)
  new.impute<-x
  old.impute <- x
  count.iter <- 1
  reach.tol <- 0
  sig <- as.matrix(var(na.exclude(x)))
  # mean.vec <- as.matrix(apply(na.exclude(x),2,mean))
  mean.vec <- matrix(c(1,1,1,1,1),nrow=5,ncol=1)
  
  while(reach.tol != 1) {
    for(i in 1:nrow(x)) {
      pick.miss <-( c( missvals[i,]) )
      if ( sum(pick.miss) != 0 ) {
        inv.S <- solve(sig[!pick.miss,!pick.miss]) # we need the inverse of the covariance
        
        # Run the EM
        new.impute[i,pick.miss] <- mean.vec[pick.miss] +
          sig[pick.miss,!pick.miss] %*%
          inv.S %*%
          (t(new.impute[i,!pick.miss])- t(t(mean.vec[!pick.miss])))
      }
    }
    
    sig <- var((new.impute))
    mean.vec <- as.matrix(apply(new.impute,2,mean))
    
    if(count.iter > 1){ # we don't want this to run on the first iteration or else if fails
      for(l in 1:nrow(new.impute)){
        for(m in 1:ncol(new.impute)){
          if( abs((old.impute[l,m]-new.impute[l,m])) > tol ) {
            reach.tol <- 0
          } else {
            reach.tol <- 1
          }
        }
      }
    }
    cat(count.iter,'\n') #for debugging
    count.iter <- count.iter+1 # used for debugging purposes to ensure process it iterating properly
    old.impute <- new.impute
  }
  
  return(new.impute)
}
straw.imputed <- EMalg.orig(straw, tol=.0001)
round(straw.imputed)
mu.est <- as.matrix(apply(straw.imputed,2,mean))
mu.est
true.mu

#plots
plot(straw.imputed[,1], straw.imputed[,2], pch=16, main="Scatterplot of Missing Data",
     sub="Missing Values in Red", xlab="X",ylab="Y")

# overlay the imputed values on the plot

plot.imputed <- straw.imputed[
  row.names(
    subset(straw, is.na( straw[,2] ) | is.na(straw[,3]) )
  ),]
points(plot.imputed[,2],plot.imputed[,3], pch=16, col='red')
