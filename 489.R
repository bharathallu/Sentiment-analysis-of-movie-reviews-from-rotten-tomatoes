rm(list=ls())
tags.data <- read.csv('C:/489_project/ml-20m/tags.csv',header=T,sep=',')
#tags.data1 <- tags.data[1:1000,]
library(tm)
library(snowballC)
library(wordcloud)
library(topicmodels)
library(ggplot2)
library(ggthemes)
library(dplyr)

corpus <-  Corpus(VectorSource(tags.data$tag)) %>%
  tm_map(tolower) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers)  %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c(stopwords('english'),'like'))

dictCorpus <- corpus
corpus <- tm_map(corpus,stemDocument, language='english')

stemCompletion2 <- function(x, dictionary) {
  
  x <- unlist(strsplit(as.character(x), " "))
  
  x <- x[x != ""]
  
  x <- stemCompletion(x, dictionary=dictionary)
  
  x <- paste(x, sep="", collapse=" ")
  
  PlainTextDocument(stripWhitespace(x))
  
}


corpus <- lapply(corpus, stemCompletion2, dictionary=dictCorpus)
corpus <- Corpus(VectorSource(corpus))

print('jobdone')


h <- TermDocumentMatrix(corpus)

m <- as.matrix(h)

v <- sort(rowSums(m),decreasing = TRUE)

d <- data.frame(word=names(v),freq=v)
d <- d %>% filter(freq<=16)

wordcloud(d$word, d$freq, min.freq = 3,
          
          max.words=100, random.order=FALSE, rot.per=0.35,
          
          colors=brewer.pal(8, "Spectral"))

### getting sentiments for tags. positve score is a positive sentiment, negative is negative sentiment

library(syuzhet)
text.corpus <- score(corpus)
syuzhet_vector <- get_sentiment(unlist(corpus), method="syuzhet")

##adding sentiment to the tags data set

tags.data$sentiment <- syuzhet_vector[1:nrow(tags.data)]

### creating a corpus takes a lot of time. Also, The sentiment for every rating is
## calculated and can be averaged by movie to get the average sentiment.
## sentiment can now be added to linear regression to predict box office collections
## sentiment can be used to create a binary variable as good or bad for movies 
## and logistic regression can be done on genres,box office, no. of ratings to predict if a movie is good or bad.


