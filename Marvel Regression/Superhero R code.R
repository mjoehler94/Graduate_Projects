# Code to Create superhero dataframe is based on original code from Nick Bertzsold and Karim Hsieh


#DATA#
# install.packages('XML')
library(XML)

#STUDIO#

# from SuperheroStudioCode.R run for Fall 2017 Project

studio <- read.csv(header=TRUE,stringsAsFactors=FALSE,text='
                   "","Title","Studio","Comic"
                   "34","Ant-Man","BV","Marvel"
                   "1","Avengers","BV","Marvel"
                   "3","Avengers: Age of Ultron","BV","Marvel"
                   "20","Batman","WB","DC"
                   "54","Batman and Robin","WB","DC"
                   "27","Batman Begins","WB","DC"
                   "32","Batman Forever","WB","DC"
                   "37","Batman Returns","WB","DC"
                   "12","Batman v Superman: Dawn of Justice","WB","DC"
                   "24","Big Hero 6","BV","Marvel"
                   "63","Blade","NL","Marvel"
                   "59","Blade II","NL","Marvel"
                   "70","Blade: Trinity","NL","Marvel"
                   "6","Captain America: Civil War","BV","Marvel"
                   "36","Captain America: The First Avenger","Par.","Marvel"
                   "19","Captain America: The Winter Soldier","BV","Marvel"
                   "77","Catwoman","WB","DC"
                   "55","Daredevil","Fox","Marvel"
                   "9","Deadpool","Fox","Marvel"
                   "25","Doctor Strange","BV","Marvel"
                   "87","Elektra","Fox","Marvel"
                   "40","Fantastic Four (2005)","Fox","Marvel"
                   "68","Fantastic Four (2015)","Fox","Marvel"
                   "49","Fantastic Four: Rise of the Silver Surfer","Fox","Marvel"
                   "51","Ghost Rider","Sony","Marvel"
                   "71","Ghost Rider: Spirit of Vengeance","Sony","Marvel"
                   "50","Green Lantern","WB","DC"
                   "11","Guardians of the Galaxy","BV","Marvel"
                   "48","Hulk","Uni.","Marvel"
                   "14","Iron Man","Par.","Marvel"
                   "15","Iron Man 2","Par.","Marvel"
                   "5","Iron Man 3","BV","Marvel"
                   "73","Kick-Ass","Lions","Marvel"
                   "84","Kick-Ass 2","Uni.","Marvel"
                   "16","Man of Steel","WB","DC"
                   "7","Spider-Man","Sony","Marvel"
                   "8","Spider-Man 2","Sony","Marvel"
                   "10","Spider-Man 3","Sony","Marvel"
                   "13","Suicide Squad","WB","DC"
                   "96","Supergirl","Sony","DC"
                   "46","Superman","WB","DC"
                   "52","Superman II","WB","DC"
                   "66","Superman III","WB","DC"
                   "94","Superman IV: The Quest for Peace","WB","DC"
                   "30","Superman Returns","WB","DC"
                   "17","The Amazing Spider-Man","Sony","Marvel"
                   "29","The Amazing Spider-Man 2","Sony","Marvel"
                   "2","The Dark Knight","WB","DC"
                   "4","The Dark Knight Rises","WB","DC"
                   "56","The Green Hornet","Sony","DC"
                   "44","The Incredible Hulk","Uni.","Marvel"
                   "81","The Punisher","Lions","Marvel"
                   "89","The Spirit","Lions","DC"
                   "47","The Wolverine","Fox","Marvel"
                   "33","Thor","Par.","Marvel"
                   "28","Thor: The Dark World","BV","Marvel"
                   "53","Watchmen","WB","DC"
                   "38","X-Men","Fox","Marvel"
                   "35","X-Men Origins: Wolverine","Fox","Marvel"
                   "39","X-Men: Apocalypse","Fox","Marvel"
                   "22","X-Men: Days of Future Past","Fox","Marvel"
                   "42","X-Men: First Class","Fox","Marvel"
                   "21","X-Men: The Last Stand","Fox","Marvel"
                   "26","X2: X-Men United","Fox","Marvel"
                   ')
studio<-studio[,-1]

#TOMATOMETER#

url2 <- "http://www.superheronation.com/2011/08/22/rotten-tomatoes-ratings-for-superhero-movies/"
# (current as of April 2016)
webpage2 <- htmlParse(url2)
tomato1a <- readHTMLTable(webpage2,which=1,header=TRUE,colClasses=c("character","numeric"),stringsAsFactors=FALSE)
tomato2a <- readHTMLTable(webpage2,which=2,header=TRUE,colClasses=c("character","numeric"),stringsAsFactors=FALSE)
tomato3a <- readHTMLTable(webpage2,which=3,header=TRUE,colClasses=c("character","numeric"),stringsAsFactors=FALSE)

# throw out average row
tomato1b <- tomato1a[-63,]
tomato2b <- tomato2a[-14,]
tomato3b <- tomato3a[-7,]

# merge for total tomatometer
tomato4 <- rbind(tomato1b,tomato2b,tomato3b)
names(tomato4)[2] <- "Tomato"

# rename movies for better merging
tomato4$Title[4] <- "Iron Man"
tomato4$Title[13] <- "The Dark Knight Rises"
tomato4$Title[28] <- "Iron Man 2"
tomato4$Title[27] <- "The Amazing Spider-Man"
tomato4$Title[36] <- "Hulk"
tomato4$Title[38] <- "X-Men: The Last Stand"
tomato4$Title[11] <- "X2: X-Men United"
tomato4$Title[32] <- "The Incredible Hulk"
tomato4$Title[58] <- "Ghost Rider: Spirit of Vengeance"
tomato4$Title[53] <- "Blade: Trinity"
tomato4$Title[43] <- "The Green Hornet"
tomato4$Title[51] <- "Punisher: War Zone"
tomato4$Title[26] <- "Avengers: Age of Ultron"
tomato4$Title[10] <- "Captain America: Civil War"
tomato4$Title[33] <- "Thor: The Dark World"

# add in newer movies and their ratings
addin<-read.table(header=TRUE,text='
                  Title Tomato
                  "Big Hero 6" 89
                  "Captain America: The Winter Soldier" 89
                  "Doctor Strange" 91
                  "Suicide Squad" 26
                  "The Amazing Spider-Man 2" 53
                  "X-Men: Apocalypse" 48
                  ')
tomato5 <- rbind(tomato4,addin)

# alphabatize titles
tomato <- tomato5[order(tomato5$Title),]


#PRODUCTION BUDGET#
setClass("AccountingNumber")
setAs("character", "AccountingNumber",function(from) as.numeric(gsub(",","",gsub("[:$:]","",from))))

# website separated into 100 subsets ...
url3 <- "http://www.the-numbers.com/movie/budgets/all"
all.pages<-seq(from=1,to=5401,by=100)
production1<-NULL
for(page in all.pages){
  webpage3 <- htmlParse(paste(url3,page,sep="/"))
  
  production1a <- readHTMLTable(webpage3,which=1,header=TRUE,colClasses=c("numeric","character","character",
                                                                          "AccountingNumber","AccountingNumber","AccountingNumber"),stringsAsFactors=FALSE)
  production1b <- production1a[-seq(2,nrow(production1a),2),]
  
  production1<-rbind(production1,production1b)
}

# keep only columns we need, rename movie to "Title"
production2a <- production1[,c(3,4)]
names(production2a) <- c("Title","Budget")

# handle some individual movies with different names across the data sets for easier merging
addin<-read.table(header=TRUE,text='
                  Title Budget
                  "Supergirl"              35000000
                  "Avengers"              225000000
                  "Batman and Robin"      125000000
                  "Fantastic Four (2005)"  87500000
                  "Fantastic Four (2015)" 120000000
                  "X2: X-Men United"      125000000
                  "Blade II"               54000000
                  ')

production3 <- rbind(production2a,addin)

# alphabatize titles
production <- production3[order(production3$Title),]


#IMDb USER RATING#

url4 <- "http://www.imdb.com/list/ls051507615/?start=1&view=compact&sort=listorian:asc&defaults=1&scb=0.7495568785816431"
webpage4 <- htmlParse(url4)

user1a <- readHTMLTable(webpage4,which=1,header=TRUE,colClasses=c("numeric","character","character","character",
                                                                  "character","numeric","numeric","character"),stringsAsFactors=FALSE)

# keep only Title and IMDb score
user2a <- user1a[,c(2,7)]
names(user2a) <- c("Title","IMDb")

# rename movies for better merging and add the amazing spider-man 2 and others that aren't on the webpage
# note: the row counter on the webpage skips row 7 !
user2a$Title[61] <- "Avengers"
user2a$Title[38] <- "Batman and Robin"
user2a$Title[25] <- "Fantastic Four (2015)"
user2a$Title[68] <- "Fantastic Four (2005)"
user2a$Title[69] <- "Fantastic Four: Rise of the Silver Surfer"
user2a$Title[44] <- "X2: X-Men United"
user2a$Title[78] <- "The Punisher (1989)"

addin2<-read.table(header=TRUE,text='
                   Title IMDb
                   "The Amazing Spider-Man 2"            6.8
                   "Big Hero 6"                          7.9
                   "Batman v Superman: Dawn of Justice"  6.8
                   "Captain America: Civil War"          8.0
                   "Suicide Squad"                       6.5
                   "X-Men: Apocalypse"                   7.2
                   "X-Men: Days of Future Past"          8.0
                   ')
user2a <- rbind(user2a,addin2)

# movies with score 0 haven't happened yet
user2b <- subset(user2a,IMDb != 0)

# alphabatize titles
user <- user2b[order(user2b$Title),]


# final merge in dataset
superhero <- merge(studio,tomato)
superhero <- merge(superhero,production)
superhero <- merge(superhero,user)

# clean up
superhero$Studio <- factor(superhero$Studio)
superhero$Studio <- relevel(superhero$Studio,"WB")
superhero$Comic <- factor(superhero$Comic)
superhero$Budget <- superhero$Budget * 10^-6
superhero$Title[33]<-"Kick ***"
superhero$Title[34]<-"Kick *** 2"

# table of summary statistics
by(superhero$IMDb,superhero$Comic,mean)
by(superhero$IMDb,superhero$Comic,sd)

by(superhero$Budget,superhero$Comic,mean)
by(superhero$Budget,superhero$Comic,sd)

by(superhero$Tomato,superhero$Comic,mean)
by(superhero$Tomato,superhero$Comic,sd)

table(superhero$Comic)

