library(twitteR)
library(ROAuth)
library(tm)
library(RColorBrewer)
library(wordcloud)

#Assign keys and tokens
ckey <-  "consumerkey"
cskey <- "consumersecretkey"
atok <- "AccessToken"
astoken <- "Access secret Token"

#Log into twitter
setup_twitter_oauth(ckey,cskey,atok,astoken)

#pull Tweets using keyword, number, language, and start date
wrestling.tweets <- searchTwitter("wwe", n = 2000, lang = 'en', since = "2017-01-01")
#Strip Retweets
wrestling.tweets <- strip_retweets(wrestling.tweets)
#Make a DF to view tweet data
tweets.df <- twListToDF(wrestling.tweets)

#Clean up text
tweets.text <- sapply(wrestling.tweets, function(x) x$getText())

tweets.text <- iconv(tweets.text, 'utf-8', 'ASCII')

#Make Corpus for modeling
myCorpus <- Corpus(VectorSource(tweets.text))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))

#Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))

#Remove punctuation and Numbers
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "",x)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#Make a TDM
term.doc.matrix <- TermDocumentMatrix(myCorpus,
                                      control = list(removePunctuation = TRUE,
                                                     stopwords=c('https','wwe', stopwords('english')),
                                                     removeNumbers=TRUE,
                                                     tolower = TRUE))
term.doc.matrix <- as.matrix(term.doc.matrix)

#Assign word frequency
word.freq <- sort(rowSums(term.doc.matrix), decreasing = T)

dm <- data.frame(word=names(word.freq), freq =word.freq)

#choose Color for cloud
pal <- brewer.pal(9, "YlGnBu")
pal <- pal[-(1:4)]

#Make Word Cloud
wordcloud(dm$word, dm$freq, min.freq = 10,random.order = F, colors = pal)
