library(plyr)
pacman::p_load("rvest","XML","stringi")

setwd("C:/Analytics/GSTData")

# Install and Activate Packages
#install.packages("twitteR", "RCurl", "RJSONIO", "stringr")
library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(base64enc)

#Library for data cleaning and analysis
require(tm)
require(qdap)
require(SnowballC)
require(RSentiment)
require(cluster)
require(fpc)



uri<-c(1:200)
dataStore<-"GST"
Title<-""
Date<-""
j<-1
k<-1

#search google for top 150 atricles
for(j in 1:2)
{
ht<-read_html('https://www.google.co.in/search?q=GST+impact+economy+india&num=120&as_sitesearch=www.financialexpress.com&start=j')
links <- ht %>% html_nodes(xpath='//h3/a') %>% html_attr('href')
list<-gsub('/url\\?q=','',sapply(strsplit(links[as.vector(grep('url',links))],split='&'),'[',1))
  if(j >1)
  {
  uri<-c(uri,list)
  uri
  }
  if(j==1)
   uri<-list
j=j+1
}
#pulling data

i<-1

for(i in 1:200)
{
url<-uri[i]
#webpage<-read_html(url)
try(webpage<-read_html(url), silent = TRUE)
description_data_html<-html_nodes(webpage,'p')
description_data_html
description_data<-html_text(description_data_html)
title_data_html<-html_nodes(webpage,'h1')
title_data<-html_text(title_data_html)
head(title_data)
x<-1
tempdata<-""
for(x in 1:70)
{
tempdata<-c(tempdata,description_data[x])
x<x+1
}
dataStore<-c(dataStore,tempdata)
Title<-c(Title,title_data)
i=i+1
}


text<-clean_text(dataStore)
ap


