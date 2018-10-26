
########################
# Matt Oehler
# Stat 535 w/ Dr. Grimshaw
# Project 1 Marvel vs DC
# 12/17/17
# R code
#######################

## Clear my workspace
rm(list=ls())

#############################
## Part 0  Data Processing ##
#############################
# save scraped data to csv
# write.csv(superhero,'cleandat.csv',row.names = F)

#set working directory
setwd("~/Matt BYU/9 Fall 2017/Stat 535/Project")
dat <- read.csv("cleandat.csv",header = T)[-1]
dat$Studio <- factor(dat$Studio, levels=c('WB','Sony','Lions','Fox',
                                          'BV','Par.','Uni.','NL'))

head(dat)
dim(dat)

#############################
## part 1 summary statistics#
#############################
#quant
summary(dat$IMDb)
summary(dat$Tomato)
summary(dat$Budget)

#quantitative variables
qv <- rbind(IMDb=summary(dat$IMDb),
            Tomato=summary(dat$Tomato),
            Budget=summary(dat$Budget))
qv
#categorical
summary(dat$Studio)
summary(Comic=dat$Comic)
studio <- rbind(Frequency=summary(dat$Studio),
                Proportion=summary(dat$Studio)/sum(summary(dat$Studio)))
round(studio,2)

comic <- rbind(Frequency=summary(dat$Comic),
               Proportion=summary(dat$Comic)/sum(summary(dat$Comic)))
round(comic,2)

tab1 <- table(dat$Studio,dat$Comic)
xtable(tab1)

#make tables
library(xtable)
print(xtable(qv,include.row.names=T))
print(xtable(comic,include.row.names=T))
print(xtable(studio,include.row.names=T))
print(xtable(table(dat$Studio,dat$Comic)))


##############################
## Part 2 EDA ################
##############################
#### 330 VERSION ANALYSIS ####

### EDA
## scatter plots
pairs(dat[,2:6])

## interaction plots
library(ggplot2)
#### comic vs budget ###
pdf('interactionplot.pdf')
ggplot(dat,aes(x=Budget,y=IMDb,color=Comic)) + geom_point() +
  ggtitle("Comic/Budget Interaction") + stat_smooth(method='lm',se=F)+ theme_bw()
interaction.plot(dat$Comic,dat$Budget,dat$IMDb)
dev.off()



### MODEL THAT WAS USED ###
superlm <- lm(IMDb~Studio+Tomato+Comic+Budget+Comic:Budget, data = dat,x=T,y=T)
summary(superlm)

#confidence intervals
ci <- confint(superlm,type = 'confidence',level=.95)
ci
xtable(ci)
xtable(summary(superlm))

### MODEL ASSUMPTIONS ###
pdf('Residuals.pdf')
plot(superlm$fitted.values,superlm$residuals,
     main='Residual Plot',xlab='Fitted Values',ylab = 'Residuals')
abline(h=0,col='red')
dev.off()

#hist of residuals
pdf('histofresiduals.pdf')
hist(stdres(superlm),freq=FALSE,
     main='Distribution of Residuals',xlab='Residuals')
curve(dnorm,from=-5,to=5,lwd=2,col="red",add=TRUE)
dev.off()
ks.test(stdres(superlm),'pnorm')

#plot of both
pdf('bothResiduals.pdf')
par(mfrow=c(1,2))
plot(superlm$fitted.values,superlm$residuals,
     main='Residual Plot',xlab='Fitted Values',ylab = 'Residuals')
abline(h=0,col='red')
hist(stdres(superlm),freq=FALSE,
     main='Distribution of Residuals',xlab='Residuals')
curve(dnorm,from=-5,to=5,lwd=2,col="red",add=TRUE)
dev.off()

#### influential observations ###
#leverage rule of thumb (2*(k+1)/n)
#in this case we have one coefficient to estimate => k = 1
k <- 12
n <- nrow(dat)
rot.l <- (2*(k+1)/n)
rot.l
#leverage rule of thumb
movies.lev <- lm.influence(superlm)$hat
lev.titles <- as.character(dat[movies.lev>rot.l,c('Title')])
lev.titles
#cooks distance rule of thumb: (4/(n-k-1))
cd <- cooks.distance(superlm)
rot.cd <- (4/(n-k-1))
rot.cd 
cd.titles <- as.character(dat[cd > rot.cd,c('Title')])
cd.titles
#r studentized rot is >2 (n=50-100), other wise we can increase it 
movies.rstudent <- rstudent(superlm)
rstud.titles <- as.character(dat[abs(movies.rstudent) > 2,c('Title')])
rstud.titles
#visualize the influential observations
pdf('ruleofthumb.pdf')
par(mfrow=c(1,3))
plot(movies.lev,main='Leverage',ylab='')
abline(h=rot.l,col='red')
plot(cd,main='Cooks Distance',ylab='')
abline(h=rot.cd,col='red')
plot(movies.rstudent,main = 'R-studentized',ylab='')
abline(h=c(-2,2),col='red')
dev.off()


#table of influential movies
inf.movies <- union(union(lev.titles,cd.titles),rstud.titles)
inf.movies
is.lev <- ifelse(inf.movies %in% lev.titles,'Yes','No')
is.cd <- ifelse(inf.movies %in% cd.titles,'Yes','No')
is.rstud <- ifelse(inf.movies %in% rstud.titles,'Yes','No')
inf.table <- cbind("Influential Movies" = inf.movies,
      "Leverage" = is.lev,
      "Cook's Distance" = is.cd,
      "R-studentized" = is.rstud)
inf.table
print(xtable(inf.table))

#look at points influential points
pdf('influencepoints.pdf')
# par(mfrow=c(1,2))
dat$is.influence <- as.factor(ifelse(dat$Title %in% inf.movies,1,0))
ggplot(dat,aes(x=Tomato,y=IMDb,color = is.influence)) + geom_point() +
  ggtitle("Influential Points") + theme_bw() -> plot1
ggplot(dat,aes(x=Budget,y=IMDb,color = is.influence)) + geom_point() +
   theme_bw() +ggtitle("Influential Points") -> plot2
grid.arrange(plot1, plot2, ncol=2)
dev.off()
#############

#######################################################################################
### TESTING HYPOTHESIS ################################################################
#######################################################################################
#Full model: IMDb~Studio+Tomato+Comic+Budget+Comic:Budget
#reduced model is the same as actual model without variable we are testing
# C <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,),byrow=T,nrow=?,ncol=12) #template for C matrix
X <- superlm$x
y <- superlm$y
bhat <- solve(t(X) %*% X) %*% t(X) %*% y
n <- nrow(X)
p <- ncol(X)
s2 <- 1/(n-p) * t(y - X %*% bhat) %*% (y - X %*% bhat)

#testing diff between marvel and DC: H0 Beta_marvel == 0
# Ccom <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,0),byrow=T,nrow=1,ncol=12)
reduced.model <- lm(IMDb~Studio+Tomato+Budget, data = dat,x=T,y=T)
anova(superlm,reduced.model)

#sanity check (is this the same as removing comic from the model???)
reduced.model <- lm(IMDb~Studio+Tomato+Budget,data=dat)
anova(superlm,reduced.model)

#testing effect of studio, test difference of all pairs
#this is close but not exactly the same as full and reduced model


#sanitycheck for studio effect
reduced.model <- lm(IMDb~Tomato+Comic+Budget+Comic:Budget, data = dat,x=T,y=T)
anova(superlm,reduced.model)

###graphic of effect of comic
pdf('comiceffect.pdf')
library(plotrix)
plotCI(superlm$coefficients['ComicMarvel'],li=ci[10,1],ui=ci[10,2],
       xlab='',ylab='Effect',main='Effect of ComicMarvel',xaxt='n')
abline(h=0,col='red')
text(.7,.2,"DC")
axis(side=1, at=1, labels="Marvel", cex=0.7)
dev.off()
###graph effect of studio
pdf('studioeffect.pdf')
labs <- c('Sony','Lions','Fox','BV','Par.','Uni.','NL')
plotCI(superlm$coefficients[2:8],li=ci[2:8,1],ui=ci[2:8,2],
       xlab='',ylab='Effect',main='Effect of Studio',xaxt='n')
axis(side=1, at=1:7, labels=labs, cex=0.7)
abline(h=0,col='red')
text(1.5,.2, 'WB')
dev.off()

###graph of both comic and studio effects
pdf('botheffects.pdf')
par(mfrow=c(2,1))
plotCI(superlm$coefficients['ComicMarvel'],li=ci[10,1],ui=ci[10,2],
       xlab='',ylab='Effect',main='Effect of ComicMarvel',xaxt='n')
abline(h=0,col='red')
text(.7,.2,"DC")
axis(side=1, at=1, labels="Marvel", cex=0.7)
labs <- c('Sony','Lions','Fox','BV','Par.','Uni.','NL')
plotCI(superlm$coefficients[2:8],li=ci[2:8,1],ui=ci[2:8,2],
       xlab='',ylab='Effect',main='Effect of Studio',xaxt='n')
axis(side=1, at=1:7, labels=labs, cex=0.7)
abline(h=0,col='red')
text(1.5,.2, 'WB')
dev.off()



#test for interaction effect

C.interaction <- matrix(c(0,0,0,0,0,0,0,0,0,0,0,1),byrow=T,ncol=12)
C.interaction
df <- nrow(C.interaction)


f.top <- t(C.interaction %*% bhat) %*% 
  solve(C.interaction %*% solve(t(X) %*% X) %*% t(C.interaction)) %*% 
  C.interaction %*% bhat
f.stat <- f.top/(df*s2)
f.stat
1 - pf(f.stat,df,n-p)


#sanitycheck for interaction effect
reduced.model <- lm(IMDb~Tomato+Comic+Studio+Budget, data = dat,x=T,y=T)
anova(superlm,reduced.model)
#it is the same! :)






####################################################################
#### SCRATCH CODE ##################################################
####################################################################

# dat$isWB <- as.factor(ifelse(dat$Studio=='WB',1,0))
# dat$isBig <- as.factor(ifelse(dat$Studio %in% c('WB','BV','Fox','Sony'),1,0))

#interaction of studio on tomato #only 3 points cause this
# int1.plot <- ggplot(dat,aes(x=Tomato,y=IMDb,color=Studio)) + geom_point() +
#                 ggtitle("Studio/Tomato Interaction")
# int1.plot + stat_smooth(method='lm',se=F) + theme_bw()
# 
# #interaction of studio on budget
# int2.plot <- ggplot(dat,aes(x=Budget,y=IMDb,color=Studio)) + geom_point() +
#   ggtitle("Studio/Budget Interaction")
# int2.plot + stat_smooth(method='lm',se=F) + theme_bw()
# interaction.plot(dat$Budget,dat$Studio,dat$IMDb)
# #interaction of isWB on Budget
# int3.plot <- ggplot(dat,aes(x=Budget,y=IMDb,color=isWB)) + geom_point() +
#   ggtitle("Budget/isWB Interaction")
# int3.plot + stat_smooth(method='lm',se=F)

##comic vs tomato
# ggplot(dat,aes(x=Tomato,y=IMDb,color=Comic)) + geom_point() +
#   ggtitle("Comic/Tomato Interaction") + stat_smooth(method='lm',se=F)+ theme_bw()
#comic vs studio #not sure if this is right
# cmap = rainbow(8)
# interaction.plot(dat$Studio,dat$Comic,dat$IMDb,col = cmap)
# interaction.plot(dat$Comic,dat$Studio,dat$IMDb,col = cmap)
# ggplot(dat,aes(x=Studio,y=IMDb,color=Comic)) + geom_point() +
#   ggtitle("Comic/Tomato Interaction") + stat_smooth(method='lm',se=F)
# #
# ggplot(dat,aes(x=Tomato,y=IMDb,color=isWB)) + geom_point() +
#   ggtitle("IsWB/Tomato Interaction") + stat_smooth(method='lm',se=F)+ theme_bw()
# #
# ggplot(dat,aes(x=Tomato,y=IMDb,color=isBig)) + geom_point() +
#   ggtitle("IsBig/Tomato Interaction") + stat_smooth(method='lm',se=F)+ theme_bw()
# 
