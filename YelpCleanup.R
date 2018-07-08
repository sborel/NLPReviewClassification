library(rjson)
library(jsonlite)
library(dplyr)
library(caret)

#business_raw <- ("/Users/SB/Documents/Capstone/YelpData/dataset/business.json")
#business <- fromJSON(sprintf("[%s]", paste(readLines(business_raw),collapse=",")))

business <- stream_in(file("/Users/SB/Documents/Capstone/YelpData/dataset/business.json"))
businessON <- business[which(business$state=="ON"),];
businessON.ID <- businessON$business_id;
rm(business)

#Pull in reviews where businesses are in ON
con_in <- file("/Users/SB/Documents/Capstone/YelpData/dataset/review.json")
con_out <- file(tmp <- tempfile(), open = "wb")
stream_in(con_in, handler = function(df){
  df <- df[which(df$business_id %in% businessON.ID),]
  stream_out(df, con_out, pagesize = 1000)
}, pagesize = 5000)
close(con_out)
reviews <- stream_in(file(tmp))
nrow(reviews)
unlink(tmp)

## Get distribution for number of reviews
hist(as.numeric(businessON$stars))

# Get User IDs of users who made reviews in the subset of data
reviewers <- unique(reviews.test$user_id)

#Pull in user information for ones that created reviews in ON
con_in <- file("/Users/SB/Documents/Capstone/YelpData/dataset/user.json")
con_out <- file(tmp <- tempfile(), open = "wb")
stream_in(con_in, handler = function(df){
  df <- df[which(df$user_id %in% reviewers),]
  stream_out(df, con_out, pagesize = 1000)
}, pagesize = 5000)
close(con_out)
users <- stream_in(file(tmp))
nrow(users)
unlink(tmp)

# Try tokenization of reviews
library("tokenizers")
library("stopwords")
#tt <- tokenize_word_stems(reviews.test$text[1:2], stopwords = stopwords::stopwords("en"))
library(text2vec)
t1 <- itoken(reviews.test$text,
             preprocessor = tolower,
             tokenizer = word_tokenizer,
             ids = reviews.test$review_id,
             preogressbar = TRUE)
vocab <- create_vocabulary(t1, stopwords = stopwords::stopwords("en"))
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(t1, vectorizer) #created a document term matrix
dim(dtm_train) # rows are the reviews and columns ar ethe terms
# for example
# dtm_train[1,which(dtm_train[1,] != 0)]


## Creating model
# fit a logistic regression model with an L1 penalty and 4 fold cross-validation.
# library(glmnet)
# NFOLDS = 4
# t1 = Sys.time()
# glmnet_classifier = cv.glmnet(x = dtm_train, y = reviews.test$stars, 
#                               family = 'multinomial', 
#                               # L1 penalty
#                               alpha = 1,
#                               # interested in the area under ROC curve
#                               type.measure = "auc",
#                               # 5-fold cross-validation
#                               nfolds = NFOLDS,
#                               # high value is less accurate, but has faster training
#                               thresh = 1e-3,
#                               # again lower number of iterations for faster training
#                               maxit = 1e3)
# print(difftime(Sys.time(), t1, units = 'sec'))

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1])
