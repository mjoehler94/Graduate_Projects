###############
# EDA 3 GENES #
# Matt Oehler #
###############

#read in the data
genes <- read.table('GeneData.txt',header=T)
head(genes)



#plot of malignance
plot(genes$Malignant)




pairs(genes[,c(1,300:307)])



