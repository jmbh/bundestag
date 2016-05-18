# jonashaslbeck@gmail.com

library(gdata)
library(plyr)
library(mgm)
library(qgraph)
library(huge)

# Data obtained from: https://www.bundestag.de/bundestag/plenum/abstimmung
# Time Period: 26.11.2014 - 14.04.2016


##### Part 1 - Recover the data ##### 

## get title data (titles and dates of all bills collected in one file)
# !!! SET WORKING DIRECTORY !!!
titles <- read.table('file_and_vote.csv', sep=',', header=TRUE)

## get voting data (downloaded from above URL)
# !!! SET WORKING DIRECTORY / data !!!
files <- list.files(getwd())
files <- files[grepl('.xls',files)]

# prepare windows batch code for xls to csv conversion
win_batch <- list()
for(i in 1:length(files)) {
  win_batch[[i]] <- paste0('XlstoCsv ', files[i], ' ', substr(files[i], 1, nchar(files[i])-4), '.csv')
}
win_batch_coll <- paste0(win_batch)
write(win_batch_coll, file='winbatch_ex.cmd') # execute in windows, has to be in same folder as data

# (any other way to convert xls to csv is also fine ...)


# load data from csv
files2 <- list.files(getwd())
files_csv <- files2[grepl('.csv', files2)]

library(readr)
vote_data <- list()
for(i in 1:length(files_csv)) {
  vote_data[[i]] <- read_csv(files_csv[i])
}


##### Part 2 - Transform Data ##### 

setwd("/Users/jmb/Dropbox/MyData/_PhD/_Blogposts/18_bundestag")

# three data parts:
# a) mapping person <-> Fraktion
# b) voting id <-> title
# c) voting id <-> voting behavior

## a) Person, Id, Fraktion

list_conform <- list()
for(i in 1:length(vote_data)) {
  list_conform[[i]] <- vote_data[[i]][,names(vote_data[[i]])!='AbgNr'] # necessary because in some datafiles we have the additional column
}
df_comb <- do.call(rbind, list_conform)
colnames(df_comb)[11] <- 'ungultig'

person_table <- ddply(df_comb, c('Name', 'Vorname'), function(x) return(as.character(x[1,4])))
colnames(person_table)[3] <- c('Fraktion')
person_table <- person_table[order(person_table$Name),] # order by surname

# Clean Party Names
grouping <- person_table$Fraktion
grouping[grouping=="DIE LINKE."] <- "DIE LINKE"
grouping[grouping==grouping[657]] <- "B90/GRUENE"
person_table$Fraktion <- grouping

# Deal with Germain umlauts

name_new1 <- gsub('\xf6', 'oe',  person_table$Name)
name_new2 <- gsub('\xe4', 'ae',  name_new1)
name_new3 <- gsub('\xfc', 'ue',  name_new2)
name_new4 <- gsub('\xd6', 'Oe',  name_new3)
name_new5 <- gsub('\xdc', 'Ue',  name_new4)
name_new6 <- gsub('\xdf', 'ss',  name_new5)
person_table$Name <- name_new6

vname_new1 <- gsub('\xf6', 'oe',  person_table$Vorname)
vname_new2 <- gsub('\xe4', 'ae',  vname_new1)
vname_new3 <- gsub('\xfc', 'ue',  vname_new2)
vname_new4 <- gsub('\xd6', 'Oe',  vname_new3)
vname_new5 <- gsub('\xdc', 'Ue',  vname_new4)
vname_new6 <- gsub('\xdf', 'ss',  vname_new5)
person_table$Vorname <- vname_new6


## b) Vote, Vote title

makeDate <- paste(substr(titles$File, 1,4), substr(titles$File, 5,6), substr(titles$File, 7,8), sep='-')
dates <- as.Date(makeDate, format="%Y-%m-%d")
titles$dates <- dates
titles$days <- dates-dates[1] # get effective time vector

## c) Voting behavior

# Coding:
# 1 = ja (yes)
# 2 = nein (no)
# 3 = enthaltung (abstention from vote)
# 4 = ungueltig (not valid)
# 5 = not present (not present)

unique_votes <- unique(paste(df_comb$Wahlperiode, df_comb$Sitzungnr, df_comb$Abstimmnr, sep = '_'))
unique_persons <- unique(paste(person_table$Name, person_table$Vorname, sep = '_'))

# storage
vote_table <- matrix(NA, length(unique_votes), nrow(person_table))

for(p in 1:length(unique_persons)) {
  
  print(p)
  # subset person
  s_p <- subset(df_comb, Name==person_table$Name[p] & Vorname==person_table$Vorname[p])
  
  for(v in 1:length(unique_votes)) {
    
    vote <- s_p[paste(s_p$Wahlperiode, s_p$Sitzungnr, s_p$Abstimmnr, sep = '_')==unique_votes[v],]
    if(nrow(vote)==0) next
    resp <- NA
    if(vote$ja==1) resp <- 1
    if(vote$nein==1) resp <- 2
    if(vote$Enthaltung==1) resp <- 3
    if(vote$ungultig==1) resp <- 4
    if(vote$nichtabgegeben==1) resp <- 5
    
    vote_table[v,p] <- resp
    
  }  
}

# save all preprocessed data

saveRDS(vote_table, file='Data_vote.X.person.RDS')
saveRDS(person_table, file='Data_persons.RDS')
saveRDS(titles, file='Data_votes.RDS')




