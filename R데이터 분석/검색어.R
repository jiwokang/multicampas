library(httr)
library(rvest)
library(XML)

#1-100
searchUrl<- "https://openapi.naver.com/v1/search/blog.xml"
Client_ID <- "izGsqP2exeThwwEUVU3x"
Client_Secret <- "WrwbQ1l6ZI"

query <- URLencode("제주여행")
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=1") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text<- xpathSApply(doc2, "//item/description", xmlValue)
text
text <- gsub("</?b>", "", text) # </?b> --> <b> 또는 </b>
text <- gsub("&[A-Za-z]+t;", "", text) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#101-200
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=101") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text2<- xpathSApply(doc2, "//item/description", xmlValue)
text2
text2 <- gsub("</?b>", "", text2) # </?b> --> <b> 또는 </b>
text2 <- gsub("&[A-Za-z]+t;", "", text2) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#201-300
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=201") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text3<- xpathSApply(doc2, "//item/description", xmlValue)
text3
text3 <- gsub("</?b>", "", text3) # </?b> --> <b> 또는 </b>
text3 <- gsub("&[A-Za-z]+t;", "", text3) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#301-400
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=301") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text4<- xpathSApply(doc2, "//item/description", xmlValue)
text4
text4 <- gsub("</?b>", "", text4) # </?b> --> <b> 또는 </b>
text4 <- gsub("&[A-Za-z]+t;", "", text4) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#401-500
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=401") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text5<- xpathSApply(doc2, "//item/description", xmlValue)
text5
text5<- gsub("</?b>", "", text5) # </?b> --> <b> 또는 </b>
text5 <- gsub("&[A-Za-z]+t;", "", text5) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#501-600
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=501") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text6<- xpathSApply(doc2, "//item/description", xmlValue)
text6
text6 <- gsub("</?b>", "", text6) # </?b> --> <b> 또는 </b>
text6 <- gsub("&[A-Za-z]+t;", "", text6) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#601-700
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=601") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text7<- xpathSApply(doc2, "//item/description", xmlValue)
text7
text7 <- gsub("</?b>", "", text7) # </?b> --> <b> 또는 </b>
text7 <- gsub("&[A-Za-z]+t;", "", text7) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

real_text = paste(text,text2,text3,text4,text5,text6,text7)

library(wordcloud)
library(RColorBrewer)
library(KoNLP)
word <- extractNoun(real_text)
cdata <- unlist(word)

cdata <- Filter(function(x) {nchar(x) < 6 & nchar(x) >= 2} ,cdata)

#밑의 키워드 제거
nouns_excluded = c("제주","여행","방문","#제주여행")
noun_final = cdata[!cdata %in% nouns_excluded]

wordcount <- table(noun_final)
wordcount <- head(sort(wordcount, decreasing=T),30)

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.5),rot.per=0.35,min.freq=1,
          random.order=F,random.color=T,colors=rainbow(20), family="dog")

#단어의 빈도수
word = sort(table(noun_final), decreasing=T)[1:50]
word

#맛집

#1-100
searchUrl<- "https://openapi.naver.com/v1/search/blog.xml"
Client_ID <- "izGsqP2exeThwwEUVU3x"
Client_Secret <- "WrwbQ1l6ZI"

query <- URLencode("연돈")
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=1") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text10<- xpathSApply(doc2, "//item/description", xmlValue)
text10
text10 <- gsub("</?b>", "", text10) # </?b> --> <b> 또는 </b>
text10 <- gsub("&[A-Za-z]+t;", "", text10) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#101-200
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=101") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text2<- xpathSApply(doc2, "//item/description", xmlValue)
text2
text2 <- gsub("</?b>", "", text2) # </?b> --> <b> 또는 </b>
text2 <- gsub("&[A-Za-z]+t;", "", text2) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#201-300
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=201") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text3<- xpathSApply(doc2, "//item/description", xmlValue)
text3
text3 <- gsub("</?b>", "", text3) # </?b> --> <b> 또는 </b>
text3 <- gsub("&[A-Za-z]+t;", "", text3) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#301-400
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=301") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text4<- xpathSApply(doc2, "//item/description", xmlValue)
text4
text4 <- gsub("</?b>", "", text4) # </?b> --> <b> 또는 </b>
text4 <- gsub("&[A-Za-z]+t;", "", text4) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#401-500
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=401") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text5<- xpathSApply(doc2, "//item/description", xmlValue)
text5
text5<- gsub("</?b>", "", text5) # </?b> --> <b> 또는 </b>
text5 <- gsub("&[A-Za-z]+t;", "", text5) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#501-600
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=501") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text6<- xpathSApply(doc2, "//item/description", xmlValue)
text6
text6 <- gsub("</?b>", "", text6) # </?b> --> <b> 또는 </b>
text6 <- gsub("&[A-Za-z]+t;", "", text6) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#601-700
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=601") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text7<- xpathSApply(doc2, "//item/description", xmlValue)
text7
text7 <- gsub("</?b>", "", text7) # </?b> --> <b> 또는 </b>
text7 <- gsub("&[A-Za-z]+t;", "", text7) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

real_text = paste(text,text2,text3,text4,text5,text6,text7)


f_word <- extractNoun(real_text)
f_data <- unlist(f_word)

f_data <- Filter(function(x) {nchar(x) < 6 & nchar(x) >= 2} ,f_data)

#밑의 키워드 제거
nouns_excluded = c("제주","여행","방문","#제주여행",'#제주맛집','&amp;','19','11','00','10','제주맛집','맛집','064','맛집이','영업시간','하게','해서','^ㅋ','안녕','휴무','30','^ㅎ^ㅎ','진짜','하나','생각','마지막','이번','#제주','#제주도','#맛집','맛집을')
nounf_final = f_data[!f_data %in% nouns_excluded]

wordcount <- table(nounf_final)
wordcount <- head(sort(wordcount, decreasing=T),30)

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.5),rot.per=0.35,min.freq=1,
          random.order=F,random.color=T,colors=rainbow(20), family="dog")

#단어의 빈도수
word = sort(table(noun_final), decreasing=T)[1:50]
word


#제주 혼밥
#1-100
searchUrl<- "https://openapi.naver.com/v1/search/blog.xml"
Client_ID <- "izGsqP2exeThwwEUVU3x"
Client_Secret <- "WrwbQ1l6ZI"

query <- URLencode("제주 식당")
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=1") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text<- xpathSApply(doc2, "//item/description", xmlValue)
text
text <- gsub("</?b>", "", text) # </?b> --> <b> 또는 </b>
text <- gsub("&[A-Za-z]+t;", "", text) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#101-200
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=101") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text2<- xpathSApply(doc2, "//item/description", xmlValue)
text2
text2 <- gsub("</?b>", "", text2) # </?b> --> <b> 또는 </b>
text2 <- gsub("&[A-Za-z]+t;", "", text2) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#201-300
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=201") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text3<- xpathSApply(doc2, "//item/description", xmlValue)
text3
text3 <- gsub("</?b>", "", text3) # </?b> --> <b> 또는 </b>
text3 <- gsub("&[A-Za-z]+t;", "", text3) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#301-400
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=301") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text4<- xpathSApply(doc2, "//item/description", xmlValue)
text4
text4 <- gsub("</?b>", "", text4) # </?b> --> <b> 또는 </b>
text4 <- gsub("&[A-Za-z]+t;", "", text4) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#401-500
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=401") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text5<- xpathSApply(doc2, "//item/description", xmlValue)
text5
text5<- gsub("</?b>", "", text5) # </?b> --> <b> 또는 </b>
text5 <- gsub("&[A-Za-z]+t;", "", text5) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#501-600
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=501") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text6<- xpathSApply(doc2, "//item/description", xmlValue)
text6
text6 <- gsub("</?b>", "", text6) # </?b> --> <b> 또는 </b>
text6 <- gsub("&[A-Za-z]+t;", "", text6) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

#601-700
url <- paste0(searchUrl, "?query=", query, "&display=100&sort=sim&start=601") #봄을 검색한 결과를 100개 출력
doc <- GET(url, add_headers("Content_Type" = "application/xml",
                            "X-Naver-client-Id" = Client_ID, "X-naver-Client-Secret" = Client_Secret))

doc2 <- xmlParse(doc, encoding="UTF-8")
text7<- xpathSApply(doc2, "//item/description", xmlValue)
text7
text7 <- gsub("</?b>", "", text7) # </?b> --> <b> 또는 </b>
text7 <- gsub("&[A-Za-z]+t;", "", text7) # &.+t; --> &at;, &abct;, &lt;, &lllt; ... &lt;, &gt;, &quot;, 

real_text = paste(text,text2,text3,text4,text5,text6,text7)

word <- extractNoun(real_text)
cdata <- unlist(word)

cdata <- Filter(function(x) {nchar(x) < 6 & nchar(x) >= 2} ,cdata)

#밑의 키워드 제거
nouns_excluded = c("제주","여행","방문","#제주여행",'#제주혼밥','맛집','00','30','혼밥을','혼밥','메뉴','^ㅎ^ㅎ','#제주맛집','#제주식당','10','11','20','식당')
noun_final = cdata[!cdata %in% nouns_excluded]

wordcount <- table(noun_final)
wordcount <- head(sort(wordcount, decreasing=T),30)

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.5),rot.per=0.35,min.freq=1,
          random.order=F,random.color=T,colors=rainbow(20), family="dog")

#단어의 빈도수
word = sort(table(noun_final), decreasing=T)[1:50]
word
