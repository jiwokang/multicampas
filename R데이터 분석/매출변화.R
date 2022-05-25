#install.packages("readxl")
library(readxl)
library(stringr)
#내국인 연도별 추이
y_credit = read_excel("Export_y.xlsx")#19~22년

y_credit = y_credit[-5] #전년동기 소비금액 제거
y_credit = y_credit[-2] #지역명 제거
y_credit = y_credit[-c(1),] #첫번째 행 제거
y_credit$소비금액 = as.numeric(y_credit$소비금액)
y_credit %>% filter(소비유형=='캠핑장/펜션')
View(y_credit)

#외국인 연도별 추이
f_credit = read_excel("Export.xlsx")#19~22년

f_credit = f_credit[-5] #전년동기 소비금액 제거
f_credit = f_credit[-2] #지역명 제거
f_credit = f_credit[-c(1),] #첫번째 행 제거
f_credit$소비금액 = as.numeric(f_credit$소비금액)

View(f_credit)

library(ggplot2)
library(dplyr)

# 내국인 관광 그래프
hospitality =y_credit %>% filter(소비유형=='기타숙박'|소비유형=='콘도'|소비유형=='호텔'|소비유형=='캠핑장/펜션')
leisure = y_credit %>% filter(소비유형=='골프장'|소비유형=='기타레저'|소비유형=='관광유원시설')
shopping = y_credit %>% filter(소비유형=='관광기념품'|소비유형=='대형쇼핑몰'|소비유형=='레저용품쇼핑'|소비유형=='면세점')
food = y_credit %>% filter(소비유형=='식음료')
camping = y_credit %>% filter(소비유형=='캠핑장/펜션')
total = y_credit %>% filter(소비유형=='총소비')

ggplot(hospitality, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("내국인 제주 숙박업 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(camping, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("내국인 제주 캠핑장/펜션 매출 추이") +
  theme(plot.title=element_text(size=20)) # 단위가 너무 작아서 그렇지 캠핑장도 매출 많이 상승함

ggplot(food, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("내국인 제주 식음료 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(leisure, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("내국인 제주 레저 및 관광지 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(shopping, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("내국인 제주 유통업 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(total, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("내국인 제주 총 관광 매출 추이") +
  theme(plot.title=element_text(size=20))



# 외국인 관광 그래프
f_hospitality =f_credit %>% filter(소비유형=='기타숙박'|소비유형=='콘도'|소비유형=='호텔'|소비유형=='캠핑장/펜션')
f_leisure = f_credit %>% filter(소비유형=='골프장'|소비유형=='기타레저'|소비유형=='관광유원시설')
f_shopping = f_credit %>% filter(소비유형=='관광기념품'|소비유형=='대형쇼핑몰'|소비유형=='레저용품쇼핑'|소비유형=='면세점')
f_food =f_credit %>% filter(소비유형=='식음료')
f_total = f_credit %>% filter(소비유형=='총소비')

ggplot(f_hospitality, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("외국인 제주 숙박업 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(f_food, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("외국인 제주 식음료업 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(f_leisure, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("외국인 제주 레저 및 관광지 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(f_shopping, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("외국인 제주 유통업 매출 추이") +
  theme(plot.title=element_text(size=20))

ggplot(f_total, aes(x=기준연도, y=소비금액, group=소비유형, color = 소비유형))+
  geom_line()+
  geom_line(size=1.2) +
  geom_point(size=3) +
  ggtitle("외국인 제주 총 관광 매출 추이") +
  theme(plot.title=element_text(size=20))

#2021년 -2019년
before =y_credit %>% filter(기준연도==2019)
after =y_credit %>% filter(기준연도==2021) 

between = after$소비금액-before$소비금액
after[ , "변화량" ] <- between
after = after[-3]
after$plus_minus <- ifelse(after$변화량 >= 0, "PLUS", "MINUS")


#그래프
ggplot(data=after, aes(x=소비유형, y=변화량, fill=plus_minus)) + 
     geom_bar(stat="identity", position="identity", colour="white", width=1) +
     scale_fill_manual(values=c("red", "blue"), guide=FALSE) +
     ggtitle("코로나 이후 매출액 변화")+
  theme(plot.title = element_text(size=25),axis.title = element_text(size = 15))



#상관분석
food[ , "동반유형" ] <- c(5.5,3.5,3.1)
food <- food[,c('소비금액','동반유형')]
cor(food)

#검색어 트랜드
trend = read_excel("datalab.xlsx")#20~22년
str(trend)
trend$날짜 = as.Date(trend$날짜)
trend$제주여행 = as.numeric(trend$제주여행)
trend$제주숙소 = as.numeric(trend$제주숙소)
trend$제주호텔 = as.numeric(trend$제주호텔)
trend$해외여행 = as.numeric(trend$해외여행)

trip = data.frame(trend$날짜,trend$제주여행)
home = data.frame(trend$날짜,trend$제주숙소)
hotel = data.frame(trend$날짜,trend$제주호텔)
f_trip = data.frame(trend$날짜,trend$해외여행)


ggplot(trip, aes(x=trend.날짜, y=trend.제주여행, group=1))+
  geom_line(colour="red")+
  geom_point(size=1) +
  ggtitle("내국인 제주 숙박업 매출 추이") +
  theme(plot.title=element_text(size=20))


ggplot() + geom_line(data=trip, aes(x=trend.날짜, y=trend.제주여행, color='red'),size=1.2)+
  geom_line(data=home, aes(x=trend.날짜, y=trend.제주숙소, color='blue'),size=1.2)+
  geom_line(data=hotel, aes(x=trend.날짜, y=trend.제주호텔, color='green'),size=1.2)+
  geom_line(data=f_trip, aes(x=trend.날짜, y=trend.해외여행, color='black'),size=1.2)+
  scale_colour_discrete(labels=c('해외여행', '제주숙소','제주호텔','제주여행'))+
  theme(legend.title = element_blank())+
  labs(title="제주여행 관련 검색량 변화",
       x ="날짜", y = "검색량")+
  theme(plot.title = element_text(hjust = 0.5, size=25),axis.title = element_text(size = 15))
























