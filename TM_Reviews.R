
library(rvest)
library(XML)
library(magrittr)

#################### Amazon Reviews #############################
aurl <- "https://www.amazon.in/Apple-iPhone-11-Pro-64GB/product-reviews/B07XLS522R/ref=cm_cr_dp_d_show_all_btm?ie=UTF8&reviewerType=all_reviews"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(aurl,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"apple.txt",row.names = F)

######### Emotion Mining ###############
library(syuzhet)
library(plotly)
library(tm)
Apple  = readLines("D:/Data Science Course/Assignments/Text Mining/apple.txt")
s_v <- get_sentences(Apple)

syuzhet <- get_sentiment(s_v, method="syuzhet")
bing <- get_sentiment(s_v, method="bing")
afinn <- get_sentiment(s_v, method="afinn")
nrc <- get_sentiment(s_v, method="nrc")
sentiments <- data.frame(syuzhet, bing, afinn, nrc)

#anger", "anticipation", "disgust", "fear", "joy", "sadness", 
#"surprise", "trust", "negative", "positive."
emotions <- get_nrc_sentiment(Apple)
emotions
emo_bar = colSums(emotions)
barplot(emo_bar)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))

plot(syuzhet, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")

# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(syuzhet)]

# and to extract the most positive sentence
positive <- s_v[which.max(syuzhet)]

