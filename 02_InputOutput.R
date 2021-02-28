#############키보드 입력#############
# scan() : 벡터 입력
# edit() : 데이터 프레임 입력

a<-scan() # 숫자를 입력(엔터). 그만 입력할 경우 빈칸에 엔터
a

b<-scan(what=character())
b

df<-data.frame()
df<-edit(df)
df


#########파일 입력########
# read.csv()
# read.table()
# read.xlsx()
setwd('../data')

student <- read.table('student.txt')
student

student1 <- read.table(file='student1.txt',header = T)
student1

student2 <- read.table(file.choose(),header = T)
student2


student3 <- read.table('student3.txt',header = T, na.strings = c("-","&","+"))
student3

# read.xlsx()
install.packages('xlsx')
library(xlsx)
