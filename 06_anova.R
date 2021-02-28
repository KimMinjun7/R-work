### 조건
# 1. 결과값이 연속변수
#     아닐 경우 : Kruskal-wallis H test
# 2. 정규 분포 여부
#     아닐 경우 : Kruskal-wallis H test
# 3. 등분산 여부
#     아닐 경우 : welch's anova
# 모든 조건을 만족하는 경우 anova를 사용하고 사후 검정은 tukey 사용


##### 사례1 : One Way ANOVA #####

library(moonBook)
View(acs)

# LDLC : 저밀도 콜레스테롤 수치 : 종속 변수
# Dx(진단 결과) : STEMI(급성심근경색), NSTEMI(만성심근경색), unstable angina(협심증) : 독립변수

moonBook::densityplot(LDLC ~ Dx, data = acs)

# 정규분포 검정
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"]))
with(acs, shapiro.test(LDLC[Dx=="STEMI"]))
with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"]))

out = aov(LDLC ~ Dx, data = acs)
out
shapiro.test(resid(out))

# 등분산 검정
bartlett.test(LDLC ~ Dx, data = acs)

# 정규분포이고 등분산일 경우
out = aov(LDLC ~ Dx, data = acs)
summary(out)

# 연속변수가 아니거나 정규분포가 아닌 경우
kruskal.test(LDLC ~ Dx, data = acs)

# 등분산이 아닐 경우
?oneway.test
oneway.test(LDLC ~ Dx, data = acs, var.equal = F)

### 사후 검정

# aov()를 사용했을 경우 : TukeyHSD()
TukeyHSD(out)

# kruskal.test()를 사용했을 경우
install.packages("pgirmess")
library(pgirmess)

kruskalmc(acs$LDLC, acs$Dx)

str(InsectSprays)
View(InsectSprays)

moonBook::densityplot(count ~ spray, data=InsectSprays)
kruskalmc(InsectSprays$count, InsectSprays$spray)

install.packages("userfriendlyscience")
library(userfriendlyscience)

posthocTGH(x=InsectSprays$spray, y=InsectSprays$count, method="games-howell")
#posthocTGH(x=acs$Dx, y=acs$LDLC, method="games-howell")


# oneway.test()를 사용했을 경우의 사후 검정
install.packages("nparcomp")
library(nparcomp)

result <- mctp(LDLC ~ Dx, data=acs)
summary(result)

##### 실습1 #####

head(iris)
# 품종별로 Sepal.width의 평균 차이가 있는가? 만약 있다면 어느 품종과 차이가 있는가?

out <- aov(Sepal.Width ~ Species, data=iris)

# 정규분포 여부
shapiro.test(resid(out))

# 등분산 여부
bartlett.test(Sepal.Width ~ Species, data=iris)

summary(out)

# 사후 검정
TukeyHSD(out)



##### 실습2 #####
mydata <- read.csv("../data/anova_one_way.csv")
View(mydata)

### 시, 군, 구에 따라서 합계 출산율의 차이가 있는가? 있다면 어느것과 차이가 있는가?

out <- aov(birth_rate ~ ad_layer, data=mydata)

shapiro.test(resid(out))

kruskal.test(birth_rate ~ ad_layer, data=mydata)

summary(out)

moonBook::densityplot(birth_rate ~ ad_layer, data=mydata)

# kruskal 일경우 사후 검정
library(pgirmess)
library(userfriendlyscience)

kruskalmc(mydata$birth_rate, mydata$ad_layer)
posthocTGH(x=mydata$ad_layer, y=mydata$birth_rate, method="games-howell")

# aov일 경우
TukeyHSD(out)


##### 실습3 #####
# 실습 데이터 : https://www.kaggle.com

telco <- read.csv("../data/Telco-Customer-Churn.csv", header=T)
View(telco)
str(telco)

# 독립변수 : paymentmethod(Bank Transfer, Credit Card, Electronic Check, Mailed Check)
# 종속변수 : Total Charges

unique(telco$PaymentMethod)

# 각 지불방식별로 갯수(인원수)와 평균 금액을 조회
telco %>% select(PaymentMethod, TotalCharges) %>% 
  group_by(PaymentMethod) %>% 
  summarise(n=n(), mean=mean(TotalCharges, na.rm=T))

moonBook::densityplot(TotalCharges ~ PaymentMethod, data=telco)

# 정규분포 여부
with(telco, shapiro.test(TotalCharges[PaymentMethod == "Bank transfer (automatic)"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod == "Credit card (automatic)"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod == "Electronic check"]))
with(telco, shapiro.test(TotalCharges[PaymentMethod == "Mailed check"]))

out <- aov(TotalCharges ~ PaymentMethod, data=telco)
shapiro.test(resid(out))

#x = telco$TotalCharges[telco$PaymentMethod == "Bank transfer (automatic)"]
#x

#x1 <- sample(x, 10, replace=F)
#shapiro.test(x1)

# 앤더슨 달링 테스트
#install.packages("nortest")
#library(nortest)

#nortest::ad.test(out)

# 등분산 테스트
bartlett.test(TotalCharges ~ PaymentMethod, data=telco)

# welch's anova
oneway.test(TotalCharges ~ PaymentMethod, data=telco, var.equal = F)

# 사후 검정
library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod, data=telco)
summary(result)

plot(result)


# 만약 정규분포가 아니라는 상황에서 테스트를 한다면
kruskal.test(TotalCharges ~ PaymentMethod, data=telco)

kruskalmc(telco$TotalCharges, telco$PaymentMethod)

library(ggplot2)
ggplot(telco, aes(telco$PaymentMethod, y=telco$TotalCharges)) +
  geom_boxplot()


##### 사례2 : Two Way ANOVA #####

mydata <- read.csv("../data/anova_two_way.csv")
View(mydata)

out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, 
           data=mydata)

shapiro.test(resid(out))

summary(out)

result = TukeyHSD(out)
plot(result)

result

ggplot(mydata, aes(birth_rate, ad_layer, col=multichild)) +
  geom_boxplot()


# 상호 변수를 제거
out <- aov(birth_rate ~ ad_layer + multichild, data=mydata)

shapiro.test(resid(out))

summary(out)

TukeyHSD(out)


##### 실습1 #####
telco <- read.csv("../data/Telco-Customer-Churn.csv", header=T)
View(telco)
str(telco)

# 독립변수 : paymentmethod, contract
# 종속변수 : Total Charges

unique(telco$Contract)

model<-aov(TotalCharges ~ PaymentMethod + Contract + PaymentMethod:Contract,
             data = telco)

model

#사후검정
result<-TukeyHSD(model)
result

x11()
plot(result)

library(ggplot2)
ggplot(telco, aes(PaymentMethod, TotalCharges, col=Contract))+geom_boxplot()




