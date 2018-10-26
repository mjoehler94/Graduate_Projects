### EDA for employee data set with many missing values

#read in the data
setwd("~/Matt BYU/10 Winter 2018/Stat 536/EDA 2 MissingData")
dat <- read.table('employee.txt',header = T)
order <- c('ID','Age','Tenure','WellBeing','JobSat','IQ','JobPerf')
dat <- dat[,order]

head(dat)
summary(dat)  
xtable(summary(dat[,-1]))

pairs(dat)

model <- lm(JobPerf ~ .,data = dat[,-1])

ci <- confint(model,type='confidence',level=0.95)
xtable(ci)
