################
rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/titanic')
getwd()
dir()
################
options(encoding="UTF-8")
localeToCharset()

RMySQL --> " res = dbGetQuery(con, "SET NAMES utf8") "

install.packages("sqldf")
library(DBI)
library(RMySQL)
db <- dbConnect(MySQL(), user="root", password="jmryu",
                dbname="FLAMINGO", host="localhost")
dbGetQuery(db, "select * from FLAMINGO.LOCALE")


