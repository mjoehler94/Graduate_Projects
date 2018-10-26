
#I coded up the coin flip example of EM on the nature.com page

#coin A gets heads with probability .7 
#coin B gets heads with probability .4
coinA.prob <- 0.7
coinB.prob <- 0.4

#randomly select a coin
#toss that coins 10x and record results
#do this until you get 50 tosses
sim_tosses <- function(a,b){
  pick_coin <- sample(c('A','B'),1)
  tosses <- matrix(-7,nrow=5,ncol=10)
  i = 1
  while(i<=5){
    if(pick_coin=='A'){
      tosses[i,] <- sample(c(1,0),10,replace= T , prob = c(a,1-a))
    }
    else{
      tosses[i,] <- sample(c(1,0),10,replace= T , prob = c(b,1-b))
    }
    i <- i + 1
  }
  tosses
}

tosses <- sim_tosses(coinA.prob,coinB.prob)

#get prob of LikA and LikB for each round of 10
#Theta.a and theta.b: guess A = 0.6, B = 0.55
theta.a = 0.6
theta.b = 0.50
prob.a <- rep(0,5)
prob.b <- rep(0,5)
for(i in 1:5){
  nheads <- sum(tosses[i,])
  trials <- ncol(tosses)
  lik.a <- dbinom(nheads,trials,theta.a) 
  lik.b <- dbinom(nheads,trials,theta.b) 
  prob.a[i] <- lik.a / (lik.a+lik.b)
  prob.b[i] <- lik.b / (lik.a+lik.b)
}
prob.a
prob.b

#compute expected number of heads and tails
prob.a[1] * sum(tosses[1,])


#compute new parameter values
prob.a[5] * sum(tosses[5,])


#################
#univariate normal case

em.norm <- function(Y,mu.init,sig.init){
  Yobs <- Y[!is.na(Y)] #get obs
  Ymis <- Y[is.na(Y)] #get missing vals
  n <- length(c(Yobs,Ymis))
  r <- length(Yobs)
  
  #initial values
  mu.t <- mu.init
  sig.t <- sig.init
  
  #function inside function
  #log-likelihood
  ll <- function(y,mu,sigma2,n){
    -0.5*n*log(2*pi*sigma2^2) - 0.5*sum((y-mu)^2)/sigma2^2
  }
  
  #compute log-likelihood for the initial values
  #ignoring the missing data
  
  lltm1 <- ll(Yobs,mu.t,sig.t,n)
  repeat{
    #E step
    EY <- sum(Yobs) + (n-r)*mu.t
    EY2 <- sum(Yobs^2) + (n-r)*(mu.t^2 + sig.t)
    
    #M step
    mu.t1 <- EY/n
    sig.t1 <- EY2/n - mu.t1^2
    
    #update parameter estimate
    mu.t <- mu.t1
    sig.t <- sig.t1
    
    # compute log-likelihood using current estimates
    # and ignoring missing data
    llt <- ll(Yobs,mu.t,sig.t,n)
    
    #print estimates for each iteration
    cat("mu est:", mu.t,' Sd est: ',sig.t, 'll: ',llt,'\n')
      
    #stop if converges
    if (abs(lltm1 -llt) < 0.001) break
    lltm1 <- llt

  }
  #fill in missing values with new mu
  return(c(mu.t,sig.t))
  
}

set.seed(1234)
x <- rnorm(20,5)
x[16:20] <- NA
x

xobs <- x[!is.na(x)]
xmis <- x[is.na(x)]
r <- length(xobs)
mu.init <- mean(xobs)
sig.init <- var(xobs)*(r-1)/r
em.norm(x,mu.init,sig.init)
Y <- x

em.norm(x,0,0.2)



