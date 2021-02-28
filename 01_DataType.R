##### 변수 #####

goods = "냉장고"


# 변수 사용시 객체 형태로 사용하는 것을 권장
goods.name = "냉장고"
goods.code = "ref001"
goods.price = 600000

goods.name

# 값을 대입할 때에는 = 보다는 <- 사용
goods.name <- "냉장고"
goods.code <- "ref001"
goods.price <- 600000

# 데이터 타입 확인
class(goods.name)
class(goods.price)
mode(goods.name)
mode(goods.price)

# 도움말 활용
help(sum)
?sum
args(sum)
example(sum)

##### 데이터 타입 #####

#* 배열 
#* ---
#* 1) 같은 형식끼리 묶어준다
#* 2) 크기가 정해져 있다.
#* 3) 삽입, 삭제가 불편

# 스칼라(0차원) : 숫자, 문자열, 불린(boolean), 팩터, NA, null
#     *.기본 자료형 : numeric, character, logical, factor
#     *.특수 자료형 : NA, NULL, NaN, Inf
# 벡터(1차원) : 기본 데이터 타입(Vector)
# 행렬(2차원) : Matrix
# 데이터프레임(2차원) : DataFrame
# 배열(3차원 이상) : Array
# 리스트(3차원 이상) : List

######################### Vector
#1) 기본 자료 구조 
#2) 1차원 배열
#3) 인덱스 접근
#4) 동일한 자료형만 저장
#5) c():combine, seq(), rep(), ...

v <- c(1, 2, 3, 4, 5)
v
class(v)
mode(v)

(v <- c(1, 2, 3, 4, 5))

mode(c(1:5))
class(c(1, 2, 3, 4, "5"))

# seq()
?seq
(seq(from=1, to=10, by=2))
(seq(1, 10, 2))

# rep()
(rep(1:3, 3))

# 벡터의 접근
a <- c(1:50)
a[10:45]
length(a)
a[10:(length(a)-5)]
a[10:length(a)-5]

b<- c(13, -5, 20:23, 12, -2:3)
b
b[1]
b[c(2, 4)]
b[c(4, 5:8, 7)]
b[-1]
b[-2]
b[-c(2, 4)]

# 집합 연산
x <- c(1, 3, 5, 7)
y <- c(3, 5)

union(x, y); setdiff(x, y); intersect(x, y)

# 컬럼명 지정
age <- c(30, 35, 40)
names(age) <- c("홍길동", "임꺽정", "신돌석")
age

# 특정 변수의 데이터 제거
age <- NULL
age

# 벡터 생성의 또다른 표현
x <- c(2, 3)
x <- (2:3)
x




########################### factor
# 범주형 데이터 타입

gender <- c("man", "women", "women", "man", "man")
gender
class(gender)
mode(gender)
is.factor(gender)
plot(gender)

ngender <- as.factor(gender)
ngender
class(ngender)
mode(ngender)
is.factor(ngender)
plot(ngender)
table(ngender)

?factor
ofactor <- factor(gender, levels=c("women", "man"), ordered=TRUE)
ofactor


########################### Matrix
# 1) 행과 열의 2차원 배열
# 2) 동일한 데이터 타입만 저장 가능
# 3) matrix(), rbind(), cbind()

m <- matrix(c(1:5))
m

m <- matrix(c(1:11), nrow=2)
m

m <- matrix(c(1:11), nrow=2, byrow=T)
m

class(m)
mode(m)

# rbind(), cbind()
x1 <- c(3, 4, 50:52)
x2 <- c(30, 5, 6:8, 7, 8)
x1
x2

mr <- rbind(x1, x2)
mr

mc <- cbind(x1, x2)
mc

# matrix 차수 확인
x <- matrix(c(1:9), ncol=3)
x

length(x); nrow(x); ncol(x); dim(x)

# 컬럼명 지정
colnames(x) <- c("one", "two", "three")
x
colnames(x)

# apply
?apply
apply(x, 1, max)
apply(x, 2, max)
apply(x, 1, sum)
apply(x, 2, sum)

# 행렬 데이터 접근
aws = read.delim("../data/AWS_sample.txt", sep="#")
head(aws)

(aws[1,1])

x1 <- aws[1:3, 2:4]
x1

# x2 <- aws[9:11, ]
x2 <- aws[9:11, 2:4]
x2

cbind(x1, x2)
rbind(x1, x2)


########################### data.frame
# 1) DB의 table과 유사
# 2) R에서 가장 많이 사용되는 구조
# 3) 컬럼 단위로 서로 다른 데이터 타입 사용 가능
# 4) data.frame(), read.csv(), read.delim(), read.table(), ...

no <- c(1, 2, 3)
name <- c("hong", "lee", "kim")
pay <- c(150, 250, 300)

emp <- data.frame(NO=no, Name=name, Payment=pay)
emp

# read.csv(), read.table()
getwd()
setwd("../data")
getwd()

txtemp <- read.table("emp.txt", header=T, sep=" ")
txtemp
class(txtemp)

csvemp <- read.csv("emp.csv")
csvemp

csvemp1 <- read.csv("emp.csv", header=T, col.names=c("사번", "이름", "급여"))
csvemp1

csvemp2 <- read.csv("emp2.csv", header=F, col.names=c("사번", "이름", "급여"))
csvemp2

View(csvemp2)

#접근
csvemp2[,1]
csvemp2$사번
class(csvemp2$사번)

#구조 확인
str(csvemp2)

#기본 통계량 확인
summary(csvemp2)

# apply
df<-data.frame(x=c(1:5),y=seq(2,10,2), z=c("a","b","c","d","e"))
df

apply(df[,c(1,2)],2,sum)
apply(df[,c(1,2)],1,sum)

# 데이터의 일부 추출
x1<- subset(df,x>=3)
x1

x2<- subset(df,x>2 & x<=6 )
x2

height<-data.frame(id=c(1,2),h=c(180,175))
weight<-data.frame(id=c(1,2),h=c(80,75))
user<-merge(height,weight, by.x='id',by.y='id')
user

#######################################array
# 1)행, 열, 면의 3차원 배열 형태의 객체 생성
# 2)array()

v<-c(1:12)
arr<-array(v,c(3,2,3))
arr
arr[,,2]
#8 추출
arr[2,1,2]


#############list
# 1)key와 value를 한쌍
# 2)python에서 dict와 유사
# 3)list()

x1<-1
x2<-data.frame(var=c(1,2,3), var2=c("a","b","c"))
x3<-matrix(c(1:12),ncol=2)
x4<-array(1:20, dim=c(2,5,2))

x5<- list(c1=x1,c2=x2,c3=x3,c4=x4)
x5

x5$c1
x5$c2

list1<-list("lee","이순신",95)
list1

list1[1]
list1[[1]]
list1[2]
list1[[2]]

# apply : lapply(), sapply()
# lapply : apply는 2차원 이상의 데이터만 입력을 받는다.
#           vector를 입력받기 위한 방법으로 사용, 반환형은 list형이다.
# sapply : 반환형이 행렬 또는 vector로 반환(lapply의 wrapper)

a<-list(c(1:5))
b<-list(c(6:10))
a
b

c<-c(a,b)
c
class(c)


x<-lapply(c,max)
x
x1<-unlist(x)
x1

y<-sapply(c,max)
y


###날짜
Sys.Date()
