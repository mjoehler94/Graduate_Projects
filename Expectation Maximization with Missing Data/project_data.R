###################################################
# Matt Oehler
# Project 2: Expectation Maximization-Missing Data
# Stat 624
# Dr. Richardson
# Fall 2017 
###################################################


### GENERAL NOTES ###

## Data Description
# relation hepatitis
# attribute Age integer [7, 78]
# attribute Sex integer [1, 2]
# attribute Steroid integer [1, 2]
# attribute Antivirals integer [1, 2]
# attribute Fatigue integer [1, 2]
# attribute Malaise integer [1, 2]
# attribute Anorexia integer [1, 2]
# attribute LiverBig integer [1, 2]
# attribute LiverFirm integer [1, 2]
# attribute SpleenPalpable integer [1, 2]
# attribute Spiders integer [1, 2]
# attribute Ascites integer [1, 2]
# attribute Varices integer [1, 2]
# attribute Bilirubin real [0.3, 8.0]
# attribute AlkPhosphate integer [26, 295]
# attribute Sgot integer [14, 648]
# attribute AlbuMin real [2.1, 6.4]
# attribute ProTime integer [0, 100]
# attribute Histology integer [1, 2]
# attribute Class {1, 2}
# inputs: Age, Sex, Steroid, Antivirals, Fatigue, Malaise,
# Anorexia, LiverBig, LiverFirm, SpleenPalpable, Spiders, 
# Ascites, Varices, Bilirubin, AlkPhosphate, Sgot, AlbuMin, ProTime, Histology
# outputs: Class

### END OF NOTES ###

#MAKE SURE THAT DIRECTORY IS CORRECT:
# setwd("~/Matt BYU/9 Fall 2017/Stat 624/Project 2")
raw_data <- readLines('hepatitis.dat')

#use regular expressions to remove description of data
linesToSkip <- grep("@",raw_data)
linesToSkip

## use regular expressions to get header names
#inputs (the covariates) #outputs (the response variable)
input.raw <- raw_data[grep("@inputs",raw_data)]
output.raw <- raw_data[grep("@outputs",raw_data)]

#split string and remove '@input' or '@output', which is the first element
input.clean <- strsplit(input.raw,' ')[[1]][-1]
# input.clean #verify
output.clean <- strsplit(output.raw,' ')[[1]][-1]
# output.clean #verify

#merge inputs and outputs #STILL HAS COMMAS IN HEADER NAMES
headers <- c(input.clean,output.clean) 
# STILL HAS COMMAS IN HEADER NAMES !!!!
headers #sanity check


# #finalized clean data set
# data.merge <- read.table(textConnection(raw_data[-linesToSkip]),sep = ',')
# data.merge[data.merge=='?']<- NA
# data.clean <- data.merge
# names(data.clean) <- headers
# data.clean

#or just hard code in the data headers
hard_headers <- c("Age", "Sex", "Steroid", "Antivirals", "Fatigue", "Malaise",
                  "Anorexia", "LiverBig", "LiverFirm", "SpleenPalpable",
                  "Spiders", "Ascites", "Varices", "Bilirubin", "AlkPhosphate",
                  "Sgot", "AlbuMin", "ProTime", "Histology", "Class")


#final data if hard coded
data.clean <- read.table(textConnection(raw_data[-linesToSkip]),sep = ',')
data.clean[data.clean=='?']<- NA
names(data.clean) <- hard_headers
head(data.clean)

# write.csv(data.clean,'hep_clean.csv')


#save under new name for convenience
hep_data <- data.clean
# View(hep_data)
110 %in% hep_data






















