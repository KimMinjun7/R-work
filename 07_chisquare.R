### 조건
# 1. Chi-Square Test : 데이터 수가 충분할 때
# 2. Fisher's exact test : 데이터 수가 부족할 때
# 3. Cochran-armitage trend test : 명목변수가 서열변수일때(trend)
# 일원 카이제곱(차이 검정), 이원 카이제곱(관계 검정)


##### 실습1 #####

View(mtcars)

# 자동차의 실린더 수와 변속기의 관계
?mtcars
table(mtcars$cyl, mtcars$am)

# 가독성을 위해 테이블 수정
mtcars$tm <- ifelse(mtcars$am == 0, "auto", "manual")
result <- table(mtcars$cyl, mtcars$tm)
result

barplot(result)

# auto의 눈금이 벗어났기 때문에 최대값을 알 수 없다.(눈금 조정)
barplot(result, ylim=c(0, 20))

# 범례 추가
barplot(result, ylim=c(0, 20), legend=rownames(result))

# 범례 형식 
mylegend = paste(rownames(result), "cyl")
barplot(result, ylim=c(0, 20), legend=mylegend)

# 그래프를 수직으로 나누기
barplot(result, ylim=c(0, 20), legend=mylegend, beside=T)

# 그래프를 수평으로
barplot(result, ylim=c(0, 20), legend=mylegend, beside=T,
        horiz=T)

# 색상
mycol = c("tan1", "coral2", "firebrick2")
barplot(result, ylim=c(0, 20), legend=mylegend, beside=T,
        horiz=T, col=mycol)

### 카이제곱 검정
result

# 결과 합
addmargins(result)

chisq.test(result)

fisher.test(result)


##### 실습2 #####
mydata <- read.csv("../data/anova_two_way.csv")
View(mydata)

# ad_layer(시군구) 와 multichild(다가구 자녀지원 조례)가 관계가 있는가?

tab <- table(mydata$ad_layer, mydata$multichild)
tab

chisq.test(tab)

fisher.test(tab)


##### 실습3 #####

library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

welfare <- read.spss(file="../data/Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

welfare <- rename(welfare, sex=h10_g3, birth=h10_g4, marriage=h10_g10,
                  religion=h10_g11, income=p1002_8aq1, code_job=h10_eco9,
                  code_region=h10_reg7)

welfare <- welfare[, c("sex", "birth", "marriage", "religion", "income",
                       "code_job", "code_region")]

# 성별과 종교의 관련성 여부

tab <- table(welfare$sex, welfare$religion)
tab

chisq.test(tab)

##### 실습4 : Cochran-Amitage Trend Test #####
library(moonBook)
View(acs)

# 흡연자, 비흡연자, 과거 흡연자와 고혈압의 유무가 서로 연관이 있을까?

table(acs$HBP, acs$smoking)

# smoking의 순서 변경
acs$smoking <- factor(acs$smoking, levels=c("Never", "Ex-smoker", "Smoker"))
result <- table(acs$HBP, acs$smoking)
result

# chisq.test(result)

?prop.trend.test
# x : 사건이 발생한 숫자
# n : 합계

# 고혈압이 발생한 사람의 숫자(x에 해당)
result[2, ]

# smoking 시도 횟수(n에 해당)
colSums(result)

prop.trend.test(result[2, ], colSums(result))

### 모자이크 그래프 시각화
mosaicplot(result)

# 색상 적용
mosaicplot(result, color=c("tan1", "firebrick2"))

# 색상표 참조
colors()

# 색상 확인
demo("colors")

# 행과 열의 위치 변경
mosaicplot(t(result), color=c("tan1", "firebrick2"))

# 레이블
mosaicplot(t(result), color=c("tan1", "firebrick2"),
           ylab="Hypertension", xlab="Smoking")


mytable(smoking ~ age, data=acs)















