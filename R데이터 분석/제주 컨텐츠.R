library(dplyr)
library(corrplot)
jeju_contents = read.csv('jeju_contests.csv')
#View(head(jeju_contents))

rest = jeju_contents %>% filter(콘텐츠분류=='음식점')
View(rest)

#인기점수를 상위 30개 음식점 정렬
g_rest = head(rest[order(-rest$인기점수),],n=30)
View(g_rest)

#30개 혼밥 점수 추가
alone = c(106,200,109,36,NA,1,NA,12,27,3,NA,13,2,NA,3,NA,3,0,62,NA,238,4,23,6,NA,28,46,NA,12,2)
g_rest = cbind(g_rest,alone)

g_rest2 = g_rest %>%
  select(평점,조회수,좋아요수,리뷰수,북마크수,SNS공유수,인기점수,alone)

View(g_rest2)
#상관계수 파악
rest_cor = cor(g_rest2,use='pairwise.complete.obs')
round(rest_cor,2)
corrplot(rest_cor, method="color", type="lower", order="hclust", tl.srt=45, diag=F)

#p-value 파악
cor.test(g_rest2$alone, g_rest2$SNS공유수) #혼밥점수와 sns 공유수의 상관관계가 있음

#평균 혼밥 점수(의미 있나?)
mean(alone,na.rm =T)



#전체데이터를 기준으로
rest2 = rest %>%
  select(평점,조회수,좋아요수,리뷰수,북마크수,SNS공유수,인기점수)
View(rest2)

#상관계수 파악
cor(rest2,use='pairwise.complete.obs')
plot(rest2)

#p-value 파악
cor.test(rest2$인기점수, rest2$SNS공유수)#둘은 상관관계가 강함!



#인기점수를 기준으로 상위 30개 관광지 정렬
tour = jeju_contents %>% filter(콘텐츠분류=='관광지')

g_tour = head(tour[order(-tour$인기점수),],n=30)
View(g_tour)




