rm(list = ls())
getwd()
setwd('/Users/jimanryu/Desktop/R/survey')
getwd()
dir()

options(encoding="UTF-8")
localeToCharset()
Sys.setlocale("LC_COLLATE","ko_kr.UTF-8")
options(encoding="UTF-8")
localeToCharset()


install.packages("ggplot2")
install.packages("sqldf")
install.packages("gridExtra")


library(help=datasets)
library(RJSONIO)

# 관련 패키지 설치
install.packages('KoNLP')
install.packages('wordcloud')
install.packages('plyr')
install.packages('twitteR')
install.packages('tm')
install.packages('Unicode')
# 설치하지 않으면 한글로된 내용을 다룰때에는 'utf8towcs'에 잘못된 입력 이라는 오류가 발생합니다
# 설치되어 있는 패키지를 로딩
library(KoNLP)
library(wordcloud)
library(plyr)
library(twitteR)
library(tm)
library(Unicode)
################




install.packages("prettyR")
install.packages("XLConnect")
install.packages("TTR")
install.packages("forecast")
install.packages("arulesViz")
install.packages("KoNLP","arules","igraph","combinat")
install.packages("arulesSequences")
install.packages("ggplot2")
install.packages('KoNLP')




#Bioconductor packages
source("http://bioconductor.org/biocLite.R")
biocLite("ctc")
biocLite("edgeR")
biocLite("DESeq")
biocLite("baySeq")
biocLite("GO.db")
biocLite("GOstats")
biocLite("biomaRt")
biocLite("Ringo")
biocLite("ShortRead")
biocLite("org.Hs.eg.db")
biocLite("goseq")
biocLite("Rsamtools")
biocLite("GenomicRanges")
biocLite("IRanges")
#CAGE analysis
biocLite("CAGEr")
#R packages
install.packages("gplots")
install.packages("ggplot2")
install.packages("snow")
install.packages("RSvgDevice")
install.packages("reshape")
#text mining
install.packages("tm")
install.packages("wordcloud")
#Twitter related
install.packages("ROAuth")
install.packages("twitteR")
#analysing sequences
install.packages("seqinr")
#Enhanced data.frame
install.packages("data.table")
#For the Riemann's Zeta function
#http://rss.acs.unt.edu/Rdoc/library/VGAM/html/zeta.html
install.packages("VGAM")
#Nonlinear regression with R
install.packages("nlrwr")
#Analysis of dose-response curves
install.packages("drc")


install.packages("parathyroid")
install.packages("Biostrings")
install.packages("ggplot2")
install.packages("nutshell")

install.packages('devtools')

install_github('KoNLP', 'haven-jeon')
install_github("roxygen")

install_github("riv","tomasgreif")

install.packages("twitteR")  
install.packages("ROAuth")
install.packages("vcd")



install_github("twitteR", username="ryujiman94")
install_github('rWBclimate', 'ropensci')


library(UsingR);library(psych)

install_github("devtools")

devtools::install_github("hadley/devtools")

install_github('quandl/R-package')

install_github("rga", "skardhamar") 

library(rga)

rga.open(instance = "ga")


install.packages("wordcloud")
install.packages("MCMCpack")
install.packages("ggmcmc")

install.packages("lubridate")
install.packages("rgl")

install.packages("rgl")
install.packages("car")
install.packages("gclus")

install.packages("iplots")
install.packages("JGR")

install.packages("timsac")
install.packages("DBI", type="source")
install.packages("RPostgreSQL", type="source")


install.packages("lubridate")
install.packages("corrplot")





library(corrplot)



devtools::install_github("rstudio/rmarkdown")
devtools::install_github("hadley/lazyeval")
devtools::install_github("hadley/dplyr")

update.packages(ask = FALSE, repos = 'http://cran.rstudio.org')

require(devtools)
library(ggplot2)

