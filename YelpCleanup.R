library(rjson)
library(jsonlite)
library(dplyr)
library(caret)

#business_raw <- ("dataset/business.json")
#business <- fromJSON(sprintf("[%s]", paste(readLines(business_raw),collapse=",")))

business <- stream_in(file("dataset/business.json"))

# Filter on businesses that are in the Canadian province of Ontario
businessON <- business[which(business$state=="ON"),];

# Filter on Businesses that are restaurants
# First explore the types of categories that could be associated with a restaurant/food serving business
categories <- data.frame(table(unlist(businessON$categories)))
# Order by the count
categ.ordered <- categories[order(categories$Freq, decreasing = TRUE),];
# Now I will manually go through and mark the categories that are restuarant related
#res.filter <- c("Restaurants", "Food", "")
# A lot of the categories are ambiguous so only the Restaurant category will be used in the initial analysis

restON <- businessON[mapply('%in%',"Restaurants",businessON$categories),];

restON.ID <- restON$business_id;
rm(business, businessON,categories,categ.ordered)

#Pull in reviews where businesses are in ON and a Restaurant
con_in <- file("dataset/review.json")
con_out <- file(tmp <- tempfile(), open = "wb")
stream_in(con_in, handler = function(df){
      df <- df[which(df$business_id %in% restON.ID),]
      stream_out(df, con_out, pagesize = 1000)
}, pagesize = 5000)
close(con_out)
reviews <- stream_in(file(tmp))
nrow(reviews)
unlink(tmp)

## Get distribution for number of reviews
table(as.numeric(reviews$stars))
hist(as.numeric(reviews$stars))
# there are no null values here
sum(is.na(as.numeric(reviews$stars)))

# Get binary score
median(as.numeric(reviews$stars))
reviews$starsbinary <- NA;
reviews$starsbinary[which(as.numeric(reviews$stars) <= 3)] <- 0;
reviews$starsbinary[which(as.numeric(reviews$stars) > 3)] <- 1;

head(reviews$stars,10)
head(reviews$starsbinary,10)


# # Get User IDs of users who made reviews in the subset of data
# reviewers <- unique(reviews$user_id)
# 
# #Pull in user information for ones that created reviews in ON
# con_in <- file("/Users/SB/Documents/Capstone/YelpData/dataset/user.json")
# con_out <- file(tmp <- tempfile(), open = "wb")
# stream_in(con_in, handler = function(df){
#       df <- df[which(df$user_id %in% reviewers),]
#       stream_out(df, con_out, pagesize = 1000)
# }, pagesize = 5000)
# close(con_out)
# users <- stream_in(file(tmp))
# nrow(users)
# unlink(tmp)

# Try tokenization of reviews
library("tokenizers")
library("stopwords")
#tt <- tokenize_word_stems(reviews.test$text[1:2], stopwords = stopwords::stopwords("en"))
library(text2vec)

# Create train and test data
library(caret)
set.seed(100)
trainIndex = createDataPartition(reviews$starsbinary, p=0.6, list=FALSE, times=1)
train = reviews[trainIndex,]
test = reviews[-trainIndex,]

t1 <- itoken(train$text,
             preprocessor = tolower,
             tokenizer = word_tokenizer,
             ids = train$review_id,
             progressbar = TRUE)
vocab <- create_vocabulary(t1, stopwords = stopwords::stopwords("en"))
vocab_pruned <- prune_vocabulary(vocab, 
                          term_count_min = 10, 
                          doc_proportion_max = 0.5,
                          doc_proportion_min = 0.001)
vectorizer = vocab_vectorizer(vocab_pruned)
dtm_train = create_dtm(t1, vectorizer) #created a document term matrix
dim(dtm_train) # rows are the reviews and columns ar ethe terms
# for example
# dtm_train[1,which(dtm_train[1,] != 0)]


## Creating model
# fit a logistic regression model with an L1 penalty and 4 fold cross-validation.
library(glmnet)
NFOLDS = 4
t1 = Sys.time()
glmnet_classifier = cv.glmnet(x = dtm_train, y = train$starsbinary,
                              family = 'binomial',
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "auc",
                              # 5-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-3,
                              # again lower number of iterations for faster training
                              maxit = 1e3)
print(difftime(Sys.time(), t1, units = 'sec'))
plot(glmnet_classifier)
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

# Testing on test data

it_test <- itoken(test$text,
             preprocessor = tolower,
             tokenizer = word_tokenizer,
             ids = test$review_id,
             progressbar = TRUE)
dtm_test = create_dtm(it_test, vectorizer)
dim(dtm_test)

preds = predict(glmnet_classifier, dtm_test, type = 'response')[,1]
glmnet:::auc(test$starsbinary, preds)

