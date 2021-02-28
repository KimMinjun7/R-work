### 목적 : 두 개의 집단이 같은지 다른지 비교하기 위해서 사용

### 조건
# 1. 결과값이 연속변수
#     아닐경우 : Mann-Whitney U test, Wilcoxen rank-sum test, 
#               Mann-whitney-wilcoxen test, MWW
# 2. 정규 분포 여부
#     아닐경우 : MWW
# 3. 등분산 여부
#     아닐경우 : Welch's t-test
#
# 4. paired t-test일 경우 Wilcoxen signed rank test

# 정규분포 : 모수적 통계 방법
# 정규분포가 아닐 경우 : 비모수적 통계 방법




##### Power Anaysis #####
# 적정한 표본의 갯수를 산출
# cohen's d

ky <- read.csv("../data/KY.csv", header=T)
View(ky)

table(ky$group)

mean.1 <- mean(ky$score[ky$group == 1])
mean.2 <- mean(ky$score[ky$group == 2])

cat(mean.1, mean.2)

sd.1 <- sd(ky$score[ky$group == 1])
sd.2 <- sd(ky$score[ky$group == 2])

cat(sd.1, sd.2)

effect_size <- abs(mean.1 - mean.2) / sqrt((sd.1^2 + sd.2^2) / 2)
effect_size

install.packages("pwr")
library(pwr)

?pwr.t.test
pwr.t.test(d=effect_size, type="two.sample",
           alternative="two.sided", power=.8, sig.level=.05)









##### 사례1 : 두 집단간의 평균 비교 #####
install.packages("moonBook")
library(moonBook)

?acs

head(acs)
str(acs)

##### 두 집단(남성과 여성)의 나이 차이를 알고 싶다.
# 귀무 가설 : 남성과 여성의 평균 나이에 대해 차이가 없다.
# 대립 가설 : 남성과 여성의 평균 나이에 대해 차이가 있다.

mean.man <- mean(acs$age[acs$sex == "Male"])
mean.woman <- mean(acs$age[acs$sex == "Female"])
cat(mean.man, mean.woman)

##### 정규 분포 테스트
moonBook::densityplot(age ~ sex, data=acs)

# 귀무가설 : 정규분포가 맞다.
# 대립가설 : 정규분포가 아니다.

shapiro.test(acs$age[acs$sex == "Male"])
shapiro.test(acs$age[acs$sex == "Female"])


##### 등분산 테스트
# 귀무가설 : 등분산이 맞다.
# 대립가설 : 등분산이 아니다.

var.test(age ~ sex, data=acs)

# MWW 검정
wilcox.test(age ~ sex, data=acs)

# t-test
t.test(age ~ sex, data=acs, var.test=T, alt="two.sided")

# Welch's test
t.test(age ~ sex, data=acs, var.test=F, alt="two.sided")


##### 사례2 : 집단이 한 개인 경우 #####

# A회사의 건전지 수명이 1000시간일때 무작위로 뽑아 10개의 건전지 수명에 대해
# 샘플이 모집단과 다르다고 할 수 있는가?

# 귀무가설 : 모집단의 평균과 같다.
# 대립가설 : 모집단의 평균과 다르다.

a <- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

shapiro.test(a)

?t.test
t.test(a, mu=1000, alt="two.sided")

# 어떤 학급의 수학 평균성적이 55점이었다.
# 0교시 수업을 하고 다시 성적을 살펴보았다.

b <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)
mean(b)

shapiro.test(b)

t.test(b, mu=55, alt="greater")


##### 사례3 : Paired Sample T-Test #####

str(sleep)
View(sleep)

before <- subset(sleep, group==1, extra)
before

after <- subset(sleep, group==2, extra)
after

install.packages("PairedData")
library(PairedData)

sleep2 <- paired(before, after)
sleep2
plot(sleep2, type="profile") + theme_bw()

# 정규분포 여부
with(sleep, shapiro.test(extra[group==1]))
with(sleep, shapiro.test(extra[group==2]))

# 등분산 여부
with(sleep, var.test(extra[group==1], extra[group==2]))

with(sleep, t.test(extra ~ group, data=sleep, paired=T))


##### 실습1 #####

# dummy : 0은 군을 나타내고, 1은 시를 나타낸다.
# 시와 군에 따라서 합계 출산율의 차이가 있는지 알아보려고 한다.
# 귀무 가설 : 차이가 없다
# 대립 가설 : 차이가 있다.

mydata <- read.csv("../data/independent.csv")
View(mydata)

gun.mean <- mean(mydata$birth_rate[mydata$dummy == 0])
si.mean <- mean(mydata$birth_rate[mydata$dummy == 1])
cat(gun.mean, si.mean)

shapiro.test(mydata$birth_rate[mydata$dummy == 0])
shapiro.test(mydata$birth_rate[mydata$dummy == 1])

wilcox.test(mydata$birth_rate ~ mydata$dummy, data=mydata)

t.test(mydata$birth_rate ~ mydata$dummy, data=mydata)


##### 실습2 #####

str(mtcars)
head(mtcars)

# am : 0은 오토, 1은 수동
# mpg : 연비
# 오토나 수동에 따라 연비가 같을까? 다를까?

a_mpg <- mean(mtcars$mpg[mtcars$am == 0])
b_mpg <- mean(mtcars$mpg[mtcars$am == 1])
cat(a_mpg, b_mpg)

shapiro.test(mtcars$mpg[mtcars$am == 0])
shapiro.test(mtcars$mpg[mtcars$am == 1])

var.test(mtcars[mtcars$am==1, 1], mtcars[mtcars$am==0, 1])

t.test(mpg ~ am, data=mtcars, var.test=T, alt="less")


##### 실습3 #####

data <- read.csv("../data/pairedData.csv")
data

# 쥐의 몸무게가 전과 후의 변화가 있는지 없는지 판단.

# 데이터를 long형으로 변경
library(reshape2)
data1 <- melt(data, id=("ID"), variable.name = "GROUP", value.name = "RESULT")
data1

# 구조를 바꾸는 또다른 방법
install.packages("tidyr")
library(tidyr)

?gather
data2 <- gather(data, key="GROUP", value="RESULT", -ID)
data2

shapiro.test(data2$RESULT[data2$GROUP == "before"])
shapiro.test(data2$RESULT[data2$GROUP == "After"])

t.test(data2$RESULT ~ data2$GROUP, data=data2, paired=T)

# 그래프로 출력

before <- subset(data2, GROUP=="before", RESULT)
before

after <- subset(data2, GROUP=="After", RESULT)
after

data3 <- paired(before, after)
plot(data3, type="profile") + theme_bw()

# 또 다른 방법
# moonBook::densityplot(RESULT ~ GROUP, data=data2)

#### 또 다른 예제

# 시 별로 2010년도와 2015년도의 출산율 차이가 있는가?
data <- read.csv("../data/paired.csv")
View(data)
str(data)

data2 <- gather(data, key="GROUP", value="RESULT", -c(ID, cities))
data2

with(data2, shapiro.test(RESULT[GROUP=="birth_rate_2010"]))
with(data2, shapiro.test(RESULT[GROUP=="birth_rate_2015"]))

wilcox.test(data2$RESULT ~ data2$GROUP, data=data2, paired=T)

# t.test(data2$RESULT ~ data2$GROUP, data=data2, paired=T)


##### 실습4 #####

# https://www.kaggle.com/kappernielsen/independent-t-test-example

mat <- read.csv("../data/student-mat.csv", header=T)
str(mat)

# 수학점수가 남학생과 여학생에 따라서 같은지 다른지 검증
# (수학점수는 G1, G2, G3에 걸쳐서 나타나 있다.)



# 같은 학생 입장에서 G1과 G3에 대해서 변화가 있었는지 확인












