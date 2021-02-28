##### 단순 회귀 분석 #####
# y = ax + b

str(women) # 미국여성을 대상으로 키와 몸무게 조사(30~39세) : 인치와 파운드
View(women)

plot(weight ~ height, data=women)

fit <- lm(weight ~ height, data=women)
fit
abline(fit, col="blue")
summary(fit)

plot(fit)

par(mfrow = c(2, 2))
plot(fit)

### 다항 회귀 분석(polynomial regression)
fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)

par(mfrow = c(1, 1))
plot(weight ~ height, data=women)
lines(women$height, fitted(fit2), col="red")

par(mfrow = c(2, 2))
plot(fit2)


### 실습1 ###

mydata <- read.csv("../data/regression.csv")
View(mydata)

# social_welfare : 사회 복지 시설
# active_firms : 사업체 수
# urban_park : 도시 공원
# doctor : 의사
# tris : 폐수 배출 업소
# kindergarten : 유치원

# 종속 변수 : birth_rate
# 독립 변수 : kindergarten
# 가설 : 유치원 수가 많은 지역에 합계 출산율도 높은가?
#     또는 합계 출산율이 유치원 수에 영향을 받는가?

attach(mydata)

fit <- lm(birth_rate ~ kindergarten, data = mydata)
summary(fit)

par(mfrow = c(2, 2))
plot(fit)

fit2 <- lm(log(birth_rate) ~ log(kindergarten), data = mydata)
summary(fit2)

plot(fit2)

shapiro.test(resid(fit))
shapiro.test(resid(fit2))

# 독립변수를 dummy로 변경(군 : 0, 시 : 1)
str(mydata)
fit3 <- lm(birth_rate ~ dummy, data=mydata)
summary(fit3)

detach(mydata)


### 실습2 ###
# www.kaggle.com : House sales price in Kings county, USA

house <- read.csv("../data/kc_house_data.csv", header=T)
View(house)

# 종속 변수 : price
# 독립 변수 : sqft_living

reg1 <- lm(price ~ sqft_living, data=house)
summary(reg1)

options("scipen"=100)
plot(reg1)

plot(house$sqft_living, house$price)

### 표준화 계수

# 종속변수 : price
# 독립변수 : sqft_living, floors, waterfront

reg1 <- lm(price ~ sqft_living + floors + waterfront, data=house)
summary(reg1)

# 표준화 계수 확인
install.packages("lm.beta")
library(lm.beta)

reg2 <- lm.beta(reg1)
summary(reg2)


### 더미 변수
# 값이 오직 "0" 과 "1"로만 이루어짐
# 이산형/범주형 변수를 연속형 변수처럼 사용
# 필요한 더미변수의 갯수는 범주의 개수 - 1


### 다중 공선성
# 원인 : 독립변수들끼리 너무 많이 겹쳐서 발생하는 문제
# 확인 방법
#   1) 산포도, 상관계수 : 상관계수가 0.9을 넘게되면 다중공선성 문제
#   2) VIF(Variance Infaltion Factor) :  분산 팽창 지수
#       - 일반적으로 10보다 크면 문제가 있다고 판단 (연속형 변수)
#       - 더미변수일 경우에는 3 이상이면 문제가 있다고 본다.
#       - sqrt(vif) > 2
#
# 해결 방법
#   1) 변수를 뺀다.
#   2) 주성분 분석





### 실습3 ###

house <- read.csv("../data/kc_house_data.csv", header=T)
View(house)

# 독립변수 : sqft_living, bathrooms, sqft_lot, floors

# 변수들 간의 상관 관계 확인
x = with(house, cbind(sqft_living, bathrooms, sqft_lot, floors))
cor(x)

cor(x, house$price)

reg1 <- lm(price ~ sqft_living, data=house)
summary(reg1)


reg2 <- lm(price ~ sqft_living + floors, data=house)
summary(reg2)


# 조절변수(interactive term, 상호변수, 교호변수)
reg2_1 <- lm(price ~ sqft_living + floors + sqft_living*floors, data=house)
summary(reg2_1)

# 다중 공선성 확인
install.packages("car")
library(car)

vif(reg2_1)

x <- with(house, cbind(floors, sqft_above, sqft_basement))
cor(x)
cor(x, house$price)

reg3 <- lm(price ~ floors + sqft_above + sqft_basement, data=house)
summary(reg3)

vif(reg3)


### 실습4 ###

View(state.x77)

states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy",
                                      "Income", "Frost")])
View(states)

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, data=states)
summary(fit)

# 다중 공선성
sqrt(vif(fit))

# 이상 관측치
#   1) 이상치(outlier) : 표준잔차 2배 이상 크거나 작은값
#   2) 큰 지레점(High leverage points) : p(절편을 포함한 인수들의 숫자)/n 의 값이
#       2~3배 이상되는 관측치 : 5/50 = 0.1
#   3) 영향 관측치(Influential Observation, Cook's D) 
#     독립변수의 수 / (샘플수 - 예측인자의 수 - 1) : 4 / (50 - 4 - 1) = 0.1
#     이 값보다 클 경우

influencePlot(fit, id=list(method="identify"))
# y축 기준 : 이상치
# x축 기준 : 큰 지레점
# 원의 크기 : 영향 관측치




### 회귀 모형의 교정

par(mfrow = c(2, 2))
plot(fit)

# 정규성을 만족하지 않을 때
powerTransform(states$Murder)
# -2, -1, -0.5, 0, 0.5, 1, 2

summary(powerTransform(states$Murder))


# 선형성을 만족하지 않을 때
boxTidwell(Murder ~ Population + Illiteracy, data=states)


# 등분산성을 만족하지 않을 때
ncvTest(fit)

spreadLevelPlot(fit)



### 회귀 모형의 선택
# Backward Stepwise Regression
# Forward Stepwise Regression
# AIC(Akaike's information criterion)

fit1 <- lm(Murder ~ ., data=states)
summary(fit1)

fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)

AIC(fit1, fit2)

#backward
full.model <- lm(Murder ~ ., data=states)
reduced.model<- step(full.model, direction = "backward", trace=0)
reduced.model


# forward
min.model <- lm(Murder ~ 1, data=states)
fwd.model <- step(min.model, direction = "forward",
                  scope=(Murder ~ Population + Illiteracy + Income + Frost))


# all subset regression
install.packages("leaps")
library(leaps)

result <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, 
                     data=states, nbest=4)
plot(result, scale="adjr2")


### 실습 4 ###

mydata <- read.csv("../data/regression.csv")
View(mydata)
str(mydata)

reg1 <- lm(birth_rate ~ cultural_center + social_welfare + active_firms +
             urban_park + doctors + tris + kindergarten + dummy, 
           data=mydata)
summary(reg1)

# cultural_center와 urban_park는 제외
reg2 <- lm(birth_rate ~ social_welfare + active_firms +
            doctors + tris + kindergarten + dummy, data=mydata)
summary(reg2)

# fordward
full.model <- lm(birth_rate ~ 1, data=mydata)
fwd.model <- step(full.model, direction = "forward",
                  scope=(birth_rate ~ cultural_center + social_welfare 
                         + active_firms + urban_park + doctors + tris 
                         + kindergarten + dummy))


plot(reg2)

# 정규성 검정
shapiro.test(resid(reg2))

library(car)
summary(powerTransform(mydata$active_firms))

reg3 <- lm(log(birth_rate) ~ log(social_welfare) + log(active_firms) +
             log(doctors) + log(tris) + log(kindergarten) + dummy, 
           data=mydata)
summary(reg3)

shapiro.test(resid(reg3))


# 다중 공선성
sqrt(vif(reg1))
sqrt(vif(reg2))
sqrt(vif(reg3))

plot(reg3)

# 등분산성
ncvTest(reg1)
ncvTest(reg2)
ncvTest(reg3)

spreadLevelPlot(reg3)


##### 로지스틱 회귀 분석 #####
# 일반화 선형 모델 : glm()

### titanic 샘플(www.kaggle.com)
titanic <- read.csv("../data/train.csv", header=T)
View(titanic)

# 종속변수 : Survived(1:생존, 0:비생존)
# 독립변수 : Pclass (1st, 2nd, 3rd) -> 더미변수로 작성

titanic$Pclass1 <- ifelse(titanic$Pclass == 1, 1, 0)
titanic$Pclass2 <- ifelse(titanic$Pclass == 2, 1, 0)
titanic$Pclass3 <- ifelse(titanic$Pclass == 3, 1, 0)
View(titanic)

reg1 <- lm(Survived ~ Pclass2 + Pclass3, data=titanic)
summary(reg1)

reg2 <- glm(Survived ~ Pclass2 + Pclass3, data=titanic, family=binomial)
summary(reg2)











