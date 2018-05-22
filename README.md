# text-analytics-with-R-
Basic wordcloud with twitter mentions
library(tm)
library(twitteR)

consumer_key = 'isXoYrcTN9QIsPZPETbqHEk9o'
consumer_secret = 'EaHsvo0am5rwAKDsCK30OsdIVKK5xc9gX1w3i22agHreLTLkBG'

setup_twitter_oauth(consumer_key,
                    consumer_secret, 
                    access_token='917278956398641152-84IBbaEjCf4XtkjXH0gwmYGP956XGBX', 
                    access_secret='CcAP6yqrVFm2euYUbDGtx7BMrsNuyaIHxEW8Uw6lqRbog')


#yes
#Authenticate

# Download feeds
# Convert to dateframe and write to CSV

# some_tweets = searchTwitter("", n=2000, lang="en")
some_tweets = searchTwitter('indigo',n=500,lang="en")
indata <- do.call("rbind", lapply(some_tweets, as.data.frame))
?lapply
?rbind
# Save file if required
str(indata)


## Create corpus that allows us to leverage TM package
Sys.setlocale("LC_ALL", "C")###
head(indata$text)
##gsub to change all the diff lang to space
for(i in 1:NROW(indata$text))   
{   
  indata$text[i] <- gsub("[^[:alnum:]' ]", " ", 
                         indata$text[i])   
} 

# Clean up non displayable characters
corpus = Corpus(VectorSource(indata$text))

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)   #  <------ 
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, 
                c('indigo','flight',
                  'http','https','t','co',
                  stopwords("english")))
stopwords("english")
#corpus = tm_map(corpus, PlainTextDocument)
library(SnowballC)
?SnowballC
### Remove plurals and variations to words
#  stopwords("english")
corpus <- tm_map(corpus, stemDocument)  
corpus <- tm_map(corpus, stripWhitespace)   
## corpus1 <- tm_map(corpus, PlainTextDocument)   


########################################################
dtm1 = DocumentTermMatrix(corpus)
dtm1
########################################################
### Calculate Positive and Negative Scores
### colnames(dtm1)[1:15]
pos.dict <- readLines("positive-words.txt")
neg.dict <- readLines("negative-words.txt")

df_dtm = as.data.frame(as.matrix(dtm1))
### write.csv(df_dtm,'dtm_before_change.csv')
for(j in 1:NROW(colnames(df_dtm)))   
{   
  #print(j)
  pos.matches <- match(colnames(df_dtm)[j], pos.dict)
  if (!is.na(pos.matches)) {
    df_dtm[,j] = +1 * df_dtm[,j]
  } 
  else {
    neg.matches <- match(colnames(df_dtm)[j], neg.dict)
    if (!is.na(neg.matches)) {
      df_dtm[,j] = -1 * df_dtm[,j]
    } 
    else {
      df_dtm[,j] = 0 
    }
  }
  
}
sentiment_score = rowSums(df_dtm)
sentiment_score
class(sentiment_score)
df_dtm$Score = sentiment_score
df_dtm$Sentiment = 'Neutral'
df_dtm$Sentiment[sentiment_score > 0] = 'Positive'
df_dtm$Sentiment[sentiment_score < 0] = 'Negative'
str(df_dtm,list.len=3000)

table(df_dtm$Sentiment)
# If you want to store the results in a file
write.csv(df_dtm,'dtm1_after_change_with_sentiment.csv')

## All done. Lets inspect the first few records
head(df_dtm$Sentiment)

######################################################
## Lets remove sparse terms and create wordclouds
## remove terms which are less occuring
nonsparse = removeSparseTerms(dtm1, .99)
nonsparse
nonsparse_df = as.data.frame(as.matrix(nonsparse))

# negative_sentiments = nonsparse_df[df_dtm$Sentiment =='Negative',!(names(df_dtm) %in% "Sentiment")]
# positive_sentiments = nonsparse_df[df_dtm$Sentiment =='Positive',!(names(df_dtm) %in% "Sentiment")]

negative_sentiments = nonsparse_df[df_dtm$Sentiment =='Negative',]
positive_sentiments = nonsparse_df[df_dtm$Sentiment =='Positive',]

library(wordcloud)
#wordStem(c("agree", "agreed","agreeing"))
wordcloud(colnames(positive_sentiments),
          colSums(positive_sentiments),
          scale=c(3,1),
          random.color=TRUE,
          colors=brewer.pal(8,"Dark2"),
          random.order=FALSE,rot.per=.25)

wordcloud(colnames(negative_sentiments),
          colSums(negative_sentiments),
          scale=c(3,1),
          random.color=TRUE,
          colors=brewer.pal(8,"Dark2"),
          random.order=FALSE,rot.per=.25)

