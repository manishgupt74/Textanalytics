# Sentiment Analysis
#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")

#set working directory ######
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


#Read the Excel file 
library(readxl)
Merged_data <- read_excel("C:/Analytics/GSTData/GST7_6_2.xls")



######## Cleaning up the data ######################
clean_text = function(x)
{
  x = gsub("rt", "", x) # remove Retweet
  x = gsub("@\\w+", "", x) # remove at(@)
  x = gsub("[[:punct:]]", "", x) # remove punctuation
  x = gsub("[[:digit:]]", "", x) # remove numbers/Digits
  x = gsub('[[:cntrl:]]', '', x) #
  x = gsub("http\\w+", "", x)  # remove links http
  x = gsub("[ |\t]{2,}", "", x) # remove tabs
  x = gsub("^ ", "", x)  # remove blank spaces at the beginning
  x = gsub(" $", "", x) # remove blank spaces at the end
  x = gsub('^\\s+|\\s+$', '', x) # remove extra space
  x <- iconv(x, 'UTF-8', 'ASCII')
  try.error = function(z) #To convert the text in lowercase
  {
    y = NA
    try_error = tryCatch(tolower(z), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(z)
    return(y)
  }
  x = sapply(x, try.error)
  return(x)
}


# create corpus
text<-clean_text(Merged_data$text)
myCorpus <- Corpus(VectorSource(text))

# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
#myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, content_transformer(stri_trans_tolower))

# Removing stop words
myStopwords <- c(stopwords("english"))
# remove 'r' and 'just' from stopwords
#myStopwords <- setdiff(myStopwords, c("r", "will" ,"get" , "watch" ,  "know" , "need" , "can" , "let", "hello", "seek" , "bend" ,
#"gst" , "help"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus<-tm_map(myCorpus,removeWords,stopwords("english"))

#Remove junk texts that starts with "askadiha" , &amp;
removerunk <- function(x) gsub("askadhia*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removerunk))
removerunk <- function(x) gsub("jampk*", "kashmir", x)
myCorpus <- tm_map(myCorpus, content_transformer(removerunk))
## keep a copy of corpus to use later as a dictionary for stem completion
removerunk <- function(x) gsub("amp", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removerunk))

#myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- tm_map(myCorpus, stemDocument,language = "english")


## keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

# The code below is used for to make text fit for paper width 
for (i in 1:5) {
  cat(paste("[[", i, "]] ", sep = ""))
  #writeLines(myCorpus[[i]])
  writeLines(as.character(myCorpus[[i]]))
}

#myCorpus <- tm_map(myCorpus, content_transformer(stemCompletion), 
#         dictionary = myCorpusCopy)
#tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
tdm<-TermDocumentMatrix(myCorpus,control = list(removePunctuation = TRUE,stopwords = c("sir", "year", stopwords("english")), removeNumbers = TRUE, tolower = FALSE))

#inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq=600))
term.freq <- rowSums(as.matrix(tdm))
term.freq
term.freq <- subset(term.freq, term.freq >=200)
df <- data.frame(term = names(term.freq), freq = term.freq)

library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()

# which words are associated with "tax"
findAssocs(tdm, "tax", 0.2)
# which words are associated with "modi"
findAssocs(tdm, "modi", 0.2)
# which words are associated with "bjp"
findAssocs(tdm, "bjp", 0.2)
# which words are associated with "growth"
findAssocs(tdm, "growth", 0.2)
# which words are associated with "price"
findAssocs(tdm, "price", 0.2)
# which words are associated with "antipeoplegst"
findAssocs(tdm, "antipeoplegst", 0.2)


#install graph and Rgraphviz packages
#source("https://bioconductor.org/biocLite.R")
#biocLite("graph")
#biocLite("Rgraphviz")

library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.12, weighting = T)


############### WORD CLOUD ######################
#Create word cloud by frequenct words
library(wordcloud)
m <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 100,
          random.order = F, colors = word.freq)

############### WORD CLOUD ######################


#####################################################################################
#Sentiment Analysis
#####################################################################################

library(RSentiment)
library(sentiment)
library(devtools)
#install_github('sentiment140', 'okugami79',force=TRUE)
library(sentiment)
library(syuzhet)

#pass cleaned text to sentiment analysis

some_txt<-text
mysentiment<-get_nrc_sentiment((some_txt))

# Get the sentiment score for each emotion
mysentiment.positive =sum(mysentiment$positive)
mysentiment.anger =sum(mysentiment$anger)
mysentiment.anticipation =sum(mysentiment$anticipation)
mysentiment.disgust =sum(mysentiment$disgust)
mysentiment.fear =sum(mysentiment$fear)
mysentiment.joy =sum(mysentiment$joy)
mysentiment.sadness =sum(mysentiment$sadness)
mysentiment.surprise =sum(mysentiment$surprise)
mysentiment.trust =sum(mysentiment$trust)
mysentiment.negative =sum(mysentiment$negative)

# Create the bar chart
yAxis <- c(mysentiment.positive,
           + mysentiment.anger,
           + mysentiment.anticipation,
           + mysentiment.disgust,
           + mysentiment.fear,
           + mysentiment.joy,
           + mysentiment.sadness,
           + mysentiment.surprise,
           + mysentiment.trust,
           + mysentiment.negative)

xAxis <- c("Positive","Anger","Anticipation","Disgust","Fear","Joy","Sadness",
           "Surprise","Trust","Negative")
colors <- c("green","red","blue","orange","red","green","orange","blue","green","red")
yRange <- range(0,yAxis)
barplot(yAxis, names.arg = xAxis, 
        xlab = "Emotional valence", ylab = "Score", main = "Twitter sentiment for GST", 
        sub = "Jul 2017", col = colors, border = "black", xpd = F, ylim = yRange,
        axisnames = T, cex.axis = 0.8, cex.sub = 0.8, col.sub = "blue")


###########Cluster Analysis#########################

tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")

plot(fit)
rect.hclust(fit, k = 4) # cut tree into 6 clusters 

m3 <- t(m2) # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed
k <- 6 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits = 3) # cluster centers

#To make it easy to find what the clusters are about, we then check the top three words in every
# cluster.
for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
  #print the tweets of every cluster
  #print(rdmTweets[which(kmeansResult$cluster==i)])
}



###########Cluster Analysis#########################

############Topic Modelling#########################
dtm <- as.DocumentTermMatrix(tdm)
library(topicmodels)


rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words
lda <- LDA(dtm.new, k = 4) # find 4 topics
term <- terms(lda, 4) # first 4 terms of every topic
term

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

# first topic identified for every document (tweet)
#require(data.table) #fore IDate

#topic <- topics(lda, 1)
#topics <- data.frame(date=as.IDate(Merged_data$created), topic)
#qplot(date, ..count.., data=topics, geom="density",
 #     fill=term[topic], position="stack")

############Topic Modelling#########################


