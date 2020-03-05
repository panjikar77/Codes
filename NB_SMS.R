
# Read SMS data

sms_data <- read.csv("D:/Data Science Course/Assignments/Naive Bayes/sms_raw_NB.csv",stringsAsFactors = F)
class(sms_data)
head(sms_data, 5)

str(sms_data)

sms_data$type <- as.factor(sms_data$type)
str(sms_data)

table(sms_data$type)

# Prepare a corpus for the text data

library(tm)
sms_corpous <- VCorpus(VectorSource(sms_data$text))
class(sms_corpous)

# Cleaning data

corpus_clean <- tm_map(sms_corpous,tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean,removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean,removePunctuation)
corpus_clean <- tm_map(corpus_clean,stripWhitespace)
corpus_clean <- tm_map(corpus_clean, PlainTextDocument)
class(corpus_clean)

#Create a document term matrix

sms_dtm <- DocumentTermMatrix(corpus_clean)
class(sms_dtm)

# Splitting data into training and test components

sms_raw_train <- sms_data[1:4169, ]
sms_raw_test  <- sms_data[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar

prop.table(table(sms_raw_train$type)) # ham ~ 86.5; spam ~ 13.5
prop.table(table(sms_raw_test$type)) # ham ~ 86.8; spam ~ 13.2

# Find words that have been referred to 5 times or more
sms_dict <- findFreqTerms(sms_dtm, 5)

# Now apply this particular dictionary of words to training and testing data.
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

dim(sms_train)
dim(sms_test)

# Create a custom function to show that if a specific word as been used more than once.
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)


##  Training a model on the data ----

# Applying naiveBayes Model on the new sms_train and original data
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

##  Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq = FALSE,prop.t = FALSE,
           prop.r = FALSE, dnn = c('predicted', 'actual'))

library(caret)
confusionMatrix(sms_test_pred,sms_raw_test$type) # Accuracy : 97.7 %


