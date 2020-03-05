library(rvest)
library(XML)
library(magrittr)

# ############# IMDB reviews Extraction ################
aurl <- "https://www.imdb.com/title/tt8579674/reviews?ref_=ttcrv_ql_3"
IMDB_reviews <- NULL
for (i in 1:10){
   murl <- read_html(as.character(paste(aurl,i,sep="=")))
   rev <- murl %>%
      html_nodes(".show-more__control") %>%
      html_text()
   IMDB_reviews <- c(IMDB_reviews,rev)
}
length(IMDB_reviews)
write.table(IMDB_reviews,"1917.txt",row.names = F)
getwd()

###### Sentiment Analysis ########

library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

text  = readLines("D:/Data Science Course/Assignments/Text Mining/1917.txt")

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
docs <- tm_map(docs, removeWords, c("film", "movie")) 
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
findAssocs(dtm, terms = "war", corlimit = 0.3)
head(d, 10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

