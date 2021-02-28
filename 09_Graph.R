##### 기본 내장 그래프 #####

# plot()
# plot(y축 데이터, 옵션)
# plot(x축 데이터, y축 데이터, 옵션)

y <- c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
plot(y)

x <- 1:10
y <- 1:10
plot(x, y)
?plot
plot(x, y, xlim=c(0, 20), ylim=c(0, 30), main="Graph",
     type="o", pch=3)

# barplot(), hist(), pie(), mosaicplot(), pair(), persp(), contour(), ...

### 그래프 배열
head(mtcars)
?mtcars
str(mtcars)

# 그래프 4개 그리기
par(mfrow=c(2, 2))
plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)
boxplot(mtcars$wt)

par(mfrow=c(1, 1))
plot(mtcars$wt, mtcars$mpg)

# 행 또는 열마다 그래프 갯수를 다르게 설정
?layout
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow=T))

plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt, mtcars$disp)
hist(mtcars$wt)

par(mfrow=c(1, 1))


### 특이한 그래프

# arrows
x <- c(1, 3, 6, 8, 9)
y <- c(12, 56, 78, 32, 9)
plot(x, y)
arrows(3, 56, 1, 12)
text(4, 40, "이것은 샘플입니다.", srt=55)

# 꽃잎 그래프
x <- c(1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 4, 5, 6, 6, 6)
y <- c(2, 1, 4, 2, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)
plot(x, y)

?sunflowerplot
z <- data.frame(x, y)
sunflowerplot(z)


# 별 그래프
# 데이터의 전체적인 윤곽을 살펴보는 그래프
# 데이터 항목에 대한 변화의 정도를 한눈에 파악
head(mtcars)
str(mtcars)
stars(mtcars[1:4], flip.labels = F,
      key.loc=c(13, 1.5), draw.segments = T)


# symbols
x <- c(1, 2, 3, 4, 5)
y <- c(2, 3, 4, 5, 6)
z <- c(10, 5, 100, 20, 10)
symbols(x, y, z)


##### ggplot2 #####
# http://www.r-graph-gallery.com/portfolio/ggplot2-package/

# 레이어 지원
#   1) 배경 설정
#   2) 그래프 추가(점, 막대, 선, ...)
#   3) 설정 추가(축 범위, 범례, 색, 표식, ....)

install.packages("ggplot2")
library(ggplot2)

### 산포도
head(mpg)

ggplot(data=mpg, aes(x=displ, y=hwy))
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point()
ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point() + xlim(3, 6) + ylim(10, 30)

ggplot(mpg, aes(displ, hwy)) + geom_point()

# midwest 데이터를 이용하여 전체인구(poptotal)와 아시아 인구(popasian) 간에
# 어떤 관계가 있는지 알아보려고 한다.
# x축은 전체인구, y축은 아시아 인구로 된 산포도를 작성
# 단, 전체인구는 30만명 이하, 아시아인구는 1만명 이하인 지역만 산포도 표시

options(scipen=99) # 지수를 숫자로 표현

ggplot(midwest, aes(poptotal, popasian)) + geom_point() + xlim(0, 300000) + ylim(0, 10000)


### 막대 그래프 : geom_col() > 막대그래프 , geom_bar() > 히스토그램
library(dplyr)

# mpg데이터에서 구동방식(drv)별로 고속도로 평균 연비를 조회하고 결과를 막대그래프로 출력
df_mpg <- mpg %>% group_by(drv) %>% summarise(mean_hwy=mean(hwy))
df_mpg

ggplot(df_mpg, aes(reorder(drv, -mean_hwy), mean_hwy)) + geom_col()
ggplot(mpg, aes(drv)) + geom_bar()
ggplot(mpg, aes(hwy)) + geom_bar()

# 어떤 회사에서 생산한 "suv" 차종의 도시 연비가 높은지 알아보려고 한다. 
# "suv"차종을 대상으로 평균 cty가 가장 높은 회사 다섯 곳을 막대그래프로 출력(막대는 연비가 높은 순으로 정렬)

df <- mpg %>% filter(class=="suv") %>% group_by(manufacturer) %>%
  summarise(mean_cty = mean(cty)) %>% arrange(desc(mean_cty)) %>% head(5)

ggplot(df, aes(x=reorder(manufacturer, -mean_cty), y=mean_cty)) + geom_col()


# 자동차 중에서 어떤 종류(class)가 가장 많은지 알아보려고 한다. 자동차 종류별 빈도를 막대 그래프로 출력
table(mpg$class)
ggplot(mpg, aes(class)) + geom_bar()


### 선 그래프 : geom_line()
str(economics)
head(economics)
tail(economics)

ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(economics, aes(date, psavert)) + geom_line()


### 상자 그래프
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()

### 참고
# 치트 시트 : Help > Cheatsheets > Data Visualization with ggplot2

# mpg데이터에서 class가 "compact", "subcompact", "suv"인 자동차의 cty가
# 어떻게 다른지 비교해 보려고 한다.
# 세 차종의 cty를 나타낸 상자 그래프를 출력
class_mpg <- mpg %>% filter(class %in% c("compact", "subcompact", "suv"))
ggplot(class_mpg, aes(class, cty)) + geom_boxplot()


##### 인터랙티브 그래프 #####
# https://plot.ly/ggplot2

install.packages("plotly")
library(plotly)

p <- ggplot(mpg, aes(displ, hwy, col=drv)) + geom_point()
ggplotly(p)

str(diamonds)
p <- ggplot(diamonds, aes(cut, fill=clarity)) + 
  geom_bar(position="dodge")
ggplotly(p)


### 시계열 데이터
install.packages("dygraphs")
library(dygraphs)

head(economics)

# data형식을 xts 타입으로 변환
library(xts)

eco <- xts(economics$unemploy, order.by=economics$date)
eco
class(eco)

dygraph(eco)

# remove.packages("plotly")

dygraph(eco) %>% dyRangeSelector()

# 저축률
eco_a <- xts(economics$psavert, order.by=economics$date)

# 실업자 수
eco_b <- xts(economics$unemploy/1000, order.by=economics$date)

eco2 <- cbind(eco_a, eco_b)
head(eco2)

colnames(eco2) <- c("psavert", "unemploy")
head(eco2)

dygraph(eco2)


##### 지도 그래프 : 단계 구분도, Choropleth map #####
install.packages("ggiraphExtra")
library(ggiraphExtra)

str(USArrests)
head(USArrests)

class(USArrests)

library(tibble)

crime <- rownames_to_column(USArrests, var="state")
str(crime)

crime$state <- tolower(crime$state)
str(crime)

library(ggplot2)

install.packages("maps")

states_map <- map_data("state")
str(states_map)

install.packages("mapproj")
?ggChoropleth
ggChoropleth(data=crime, aes(fill=Murder, map_id=state), 
             map=states_map, interactive = T)


### 대한민국 지도 만들기
# https://github.com/cardiomoon/kormaps2014

install.packages("stringi")
library()
install.packages("devtools")

devtools::install_github("cardiomoon/kormaps2014", force=T)
library(kormaps2014)

head(korpop1)
str(korpop1)

head(kormap1)
str(kormap1)

# 문자 코드 변경
korpop1 <- changeCode(korpop1)
head(korpop1)
str(korpop1)

# 컬럼 이름 변경
library(dplyr)
korpop1 <- rename(korpop1, pop="총인구_명", name="행정구역별_읍면동")
str(korpop1)

library(ggplot2)
library(ggiraphExtra)

ggChoropleth(data=korpop1, aes(fill=pop, map_id=code, tooltip=name),
             map=kormap1, interactive = T)


# 결핵환자 데이터
str(tbc)
head(tbc)


ggChoropleth(data=tbc, aes(fill=NewPts, map_id=code, tooltip=name),
             map=kormap1, interactive = T)


##### Text Mining #####

install.packages("rJava")
# rJava삭제, Sys.setenv(JAVA_HOME='경로'), rJava 다시 설치

install.packages("memoise")
#install.packages("KoNLP") # 현재는 직접 설치가 안됨

# Rtools 설치
install.packages("multilinguer")
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

library(KoNLP)

#형태소 분석을 위한 단어사전
useNIADic()

extractNoun('아버지가방에들어가신다')
extractNoun("대한민국의 영토는 한반도와 그 부속도서로 한다.")

#데이터 준비
txt<- readLines("../data/hiphop.txt")
head(txt)
class(txt)

#특수문자 제거
library(stringr)
txt<-str_replace_all(txt,"\\W"," ")
head(txt,500)


#명사 추출
nouns<-extractNoun(txt)
class(nouns)
nouns[1:10]

#list를 벡터로 변환해서 빈도수 구하기
wordcount<- table(unlist(nouns))
wordcount[50:60]

#데이터 프레임으로 변환
df_word<-as.data.frame(wordcount,stringsAsFactors = F) #문자열을 Fator로 바꾸지 말아라
head(df_word,10)
str(df_word)

#변수명 바꾸기
df_word<-rename(df_word,word=Var1, freq=Freq)
str(df_word)

#두 글자 이상 단어 추출
df_word <- filter(df_word,nchar(word)>=2)
head(df_word,100)

#top 20확인
df_word %>% arrange(desc(freq)) %>%head(20)

#wordcloud로 확인
install.packages("wordcloud")
library(wordcloud)

#난수 고정
set.seed(1234)
x11()
wordcloud(words=df_word$word, freq=df_word$freq, min.freq = 2,scale=c(4,0.3),
          color = brewer.pal(8,'Dark2'),random.order = F)                 
warnings()



twitter<-readLines('../data/twitter.csv',encoding = 'utf8')
head(twitter)
