# 필요 패키지 설치
install.packages("twitteR")
install.packages("KoNLP")
install.packages("wordcloud")
install.packages("plyr")

# Library 로드
library(twitteR)
library(KoNLP)
library(wordcloud)
library(plyr)

api_key <- "rde9B2jgZltWRmRKJKnNKPYgz"

api_secret_key <- "Hoo1RXwxJXqevG8ncUCRLZ1wo32hKr4a9PFpdmBA5Z5fWN8dUx"

access_token <- "1513429187163750401-VUsEIR2Rt4whqSDFazLNqz62LBDC3h"
access_token_secret <- "RFyXl6kiLHqkcucZhBuR8d2keavUpDf7NSTphjX8mdfKn"
options(httr_oauth_cache = TRUE)

setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)
getCurRateLimitInfo()

string <- '제주여행'
string <- iconv(string, 'CP949', 'UTF8')
tweets <- searchTwitter(searchString = string, n = 10000, lang="ko", retryOnRateLimit = 10000)
tweets
text_extracted <- sapply(tweets, function(t) t$getText())
text_extracted


word <- extractNoun(text_extracted)
cdata <- unlist(word)

cdata <- Filter(function(x) {nchar(x) < 6 & nchar(x) >= 2} ,cdata)

#밑의 키워드 제거
nouns_excluded = c("제주","여행","방문","#제주여행",'#조건만남','https','마사지','#가격비교','조건','사이트','출장','여자','일탈','만남','섹시')
noun_final = cdata[!cdata %in% nouns_excluded]

wordcount <- table(noun_final)
wordcount <- head(sort(wordcount, decreasing=T),30)

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.5),rot.per=0.35,min.freq=1,
          random.order=F,random.color=T,colors=rainbow(20), family="dog")

#단어의 빈도수
word = sort(table(noun_final), decreasing=T)[1:50]
word


string <- '제주맛집'
string <- iconv(string, 'CP949', 'UTF8')
tweets <- searchTwitter(searchString = string, n = 10000, lang="ko", retryOnRateLimit = 10000)
tweets
text_extracted <- sapply(tweets, function(t) t$getText())
text_extracted


word <- extractNoun(text_extracted)
cdata <- unlist(word)

cdata <- Filter(function(x) {nchar(x) < 6 & nchar(x) >= 2} ,cdata)

#밑의 키워드 제거
nouns_excluded = c("제주","여행","방문","#제주여행",'#조건만남','https','마사지','#가격비교','조건','사이트','출장','여자','일탈','만남','섹시')
noun_final = cdata[!cdata %in% nouns_excluded]

wordcount <- table(noun_final)
wordcount <- head(sort(wordcount, decreasing=T),30)

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.5),rot.per=0.35,min.freq=1,
          random.order=F,random.color=T,colors=rainbow(20), family="dog")

#단어의 빈도수
word = sort(table(noun_final), decreasing=T)[1:50]
word
