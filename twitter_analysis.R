##install.packages("twitteR") #installs TwitteR
##install.packages("ROAuth")
library(twitteR)
library(ROAuth)

setwd("G:/R_Training/twitter")

api_key <- "lzn8rgQq4yWHeMdfkwgUODXJJ" #in the quotes, put your API key 
api_secret <- "4lRcNNiFiNtjMuHGbH3zXoxc3gLmXEE8uhehU4ycArlXGClklg" #in the quotes, put your API secret token 
token <- "141480251-gD5IBUrnGKpyw9IpQ1y8As0xWTzpG2e1tY7IPSMl" #in the quotes, put your token
token_secret <- "oxsJD3zOlhHcwpJlSc8SM3TfilWsm1jwNdpFpfYxDFw47" #in the quotes, put your token secret

setup_twitter_oauth(api_key, api_secret, token, token_secret)

tweets <- searchTwitter("#Patanjali", n = 2000, lang = "en")

tweets.df <-twListToDF(tweets)

write.csv(tweets.df, "Patanjali.csv") 

#loading libraries
library('stringr')
library('readr')
library('wordcloud')
library('tm')
library('SnowballC')
library('RWeka')
library('RSentiment')
library('DT')

#extracting relevant data
r1 = as.character(tweets.df$text)


#Data Preprocessing
set.seed(100)
sample = sample(r1, (length(r1)))
corpus = Corpus(VectorSource(list(sample)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('english'))
corpus = tm_map(corpus, stemDocument)
dtm_up = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_up <- colSums(as.matrix(dtm_up))


#Calculating Sentiments
sentiments_up = calculate_sentiment(names(freq_up))
sentiments_up = cbind(sentiments_up, as.data.frame(freq_up))
sent_pos_up = sentiments_up[sentiments_up$sentiment == 'Positive',]
sent_neg_up = sentiments_up[sentiments_up$sentiment == 'Negative',]


wordcloud(sent_pos_up$text,sent_pos_up$freq,min.freq=2,random.order=FALSE,colors=brewer.pal(6,"Dark2"))

wordcloud(sent_neg_up$text,sent_neg_up$freq,min.freq=2,random.order=FALSE,colors=brewer.pal(6,"Dark2"))
