rm(list = ls())

setwd('..')

suppressMessages(library(rvest))
suppressMessages(library(stringr))
suppressMessages(library(xlsx))
suppressMessages(library(dplyr))
suppressMessages(library(readr))

options(stringsAsFactors = FALSE)

#source('part 1.R') #run only on the 5th of every month
rates <- read_csv("rates.csv")
rates<-as.data.frame(rates)
source('chase.R')
source('barclay.R')
source('citi.R')
source('amex.R')
source('amexbus.R')
source('BOA.R')
source('US.R')
source('part 2.R')
source('email.R')



# library('RPostgreSQL')
# pg = dbDriver("PostgreSQL")
# # run postgresql by typing in cmd: postgres -D "C:\Program Files\PostgreSQL\9.6\data"
# con = dbConnect(pg, user="user", password="password",
#                 host="localhost", port=5432, dbname="CreditCards")
# 
# Encoding(cards$IntroOffer) <- "UTF-8"
# cards$IntroOffer<- iconv(x, "UTF-8", "UTF-8",sub='')
# 
# if(dbExistsTable(con,"CardDetals")) {dbRemoveTable(con,"CardDetals")}
# dbWriteTable(con,'CardDetals', cards, row.names=FALSE)
# 
# currentCards <- cards[!is.na(cards[18]), c(1:18)]
# names(currentCards)[length(currentCards)] <- 'Value'
# if(dbExistsTable(con,"CurrentCards")) {dbRemoveTable(con,"CurrentCards")}
# dbWriteTable(con,'CurrentCards', currentCards, row.names=FALSE)





