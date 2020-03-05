#devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0")
library("twitteR")
#install.packages("ROAuth")
library("ROAuth")

cred <- OAuthFactory$new(consumerKey='2u1YoldtZAt9mJt4rvYLBA6rr', # Consumer Key (API Key)
                         consumerSecret='rPQZwMP07uc2sE8I7cwFP8Xi83rzg1Ll9XaU5UBx2VVyYQOSMy', #Consumer Secret (API Secret)
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

#install.packages("base64enc")
library(base64enc)

#install.packages("httpuv")
library(httpuv)

setup_twitter_oauth("2u1YoldtZAt9mJt4rvYLBA6rr", # Consumer Key (API Key)
                    "rPQZwMP07uc2sE8I7cwFP8Xi83rzg1Ll9XaU5UBx2VVyYQOSMy", #Consumer Secret (API Secret)
                    "1220986997134610432-eG1xPOQkpkQX9jUYpwtVfrvPByJgeG",  # Access Token
                    "4pn2H4UclHRRNfhUCAUWEBoUArzJqfbcChqTe0CFioMRf")  #Access Token Secret

#registerTwitterOAuth(cred)

Tweets <- userTimeline('MKBHD', n = 1000,includeRts = T)
TweetsDF <- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
Twitter <- TweetsDF %>% select(text)
View(Twitter)
write.csv(Twitter, "Tweets.csv",row.names = F)

getwd()
# 
#handleTweets <- searchTwitter('DataScience', n = 10000)
# handleTweetsDF <- twListToDF(handleTweets)
# dim(handleTweetsDF)
# View(handleTweetsDF)
# #handleTweetsMessages <- unique(handleTweetsDF$text)
# #handleTweetsMessages <- as.data.frame(handleTweetsMessages)
# #write.csv(handleTweetsDF, "TefalHandleTweets.csv")
# library(rtweet)

######### Sentiment Analysis ###############

library(dplyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


text <- readLines("D:/Data Science Course/Assignments/Text Mining/Tweets.csv")

docs <- Corpus(VectorSource(text))
inspect(docs)
toSpace <- function (x , pattern ) gsub(pattern, " ", x)
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, tolower)
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("mkbhd", "marcus", "brownlee", "tldtoday", "https", "tco")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
#docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
a <- as.matrix(dtm)
b <- sort(rowSums(a),decreasing=TRUE)
d <- data.frame(word = names(b),freq=b)
wordcloud(words = d$word, freq = d$freq, min.freq = 0,
          max.words=1000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(1, "Dark2"))
findFreqTerms(dtm, lowfreq = 8)
findAssocs(dtm, terms = "phone", corlimit = 0.3)
head(d, 10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


