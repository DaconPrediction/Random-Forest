setwd("D:/Rstudy/produce_data")
library(lubridate) #날짜 연산 package
library(caret)
tr <- read.csv("public_data/train.csv")
tr <- tr[,-2]
tr$date <- as.Date(tr$date)

#nmae측정 함수
nm <- function(prediction, answer){
  nmae <- abs((prediction-answer)/answer)
  out <- c() #Inf인 값들의 index
  for(i in 1:38){
    if(nmae[i]==Inf) {
      out <- c(out, i)
    }
  }
  nmae <- nmae[-c(out)]
  score <- rowMeans(nmae)
  return(score)
}

######train데이터 준비######
#n=1~21-> 품목 개수 (1주뒤 예측) 모델 return 

#train할 날짜
date <- c(seq(as.Date('2017/09/28','%Y/%m/%d'), 
              as.Date('2017/12/01','%Y/%m/%d'),1),
          seq(as.Date('2018/09/28','%Y/%m/%d'), 
              as.Date('2018/12/01','%Y/%m/%d'),1))
tr_date <- matrix(rep(0,len=4030),nrow=31,ncol=130)    #ncol = 65일(기간) * 2, len = 31 * 130
#날짜data포함
tr_date <- as.data.frame(tr_date)
colnames(tr_date) <- date
#train할 날짜데이터를 가지는 DF->tr_date
for(i in 1:130){
  a <- c()
  day <- c()
  day <- date[i]
  a <- date[i]+7 #label값 = 예측할 몇주 후 (7 = 1주뒤)
  a <- c(a,date[i]) #(당일)
  a <- c(a,day-years(1)) #(1년전)
  a <- c(a, day-seq(1,28,1)) #(하루 전부터~ 21일 전까지)
  tr_date[,i] <- a
}
rownames(tr_date) <- c('label','today','1year',1:28)
tr_date <- t(tr_date) #행열 뒤집기
tr_date <- as.data.frame(tr_date)
#가격데이터를 담은 DF
tr_price <- matrix(rep(0,len=4030),nrow=130,ncol=31) 
tr_price <- as.data.frame(tr_price)
#train할 가격데이터를 가지는 DF->tr_price
n <- 1 #농산물 번호(순서대로)
for(i in 1:130){
  for(j in 1:31){
    tr_price[i,j] <- tr[tr_date[i,j]==tr$date,1+2*n] 
    #1+2*n = n에 해당하는 농산물 가격 
  }
}
subset <- subset(tr,tr[,1+2*n]!=0) #데이터 추출
laplace <- mean(subset[,1+2*n])
for(i in 1:130){
  for(j in 1:31){
    #0을 포함하는 행을 제거하기 위해 행번호 수집
    if(tr_price[i,j]==0){
      tr_price[i,j] <- laplace
    }
  }
} 
#0을 포함하는 행을 tr 데이터에서 미리 제거를 해야 할듯
colnames(tr_price) <-c('label','today','1year',1:28)
rownames(tr_price) <- date


#랜덤 포레스트
library(randomForest)
set.seed(300)
attach(tr_price)
model <- randomForest(label~.,  data = tr_price, mtry = floor(sqrt(30)), ntree = 500) #다중 선형 회귀 모델 생성
summary(model)

# 아래 부분은 오류남
# m <- stepAIC(model) #적합한변수 선별
# m
# summary(m) #model보다 Adjusted R-squared가 더 높게 나옴
pred <- predict(model,tr_price[,2:31])
print(cor(pred,tr_price$label)) # 0.9766225 나옴


######test######
library(psych)#산포도와 상관관계, 히스토그램 등을 모두 보여주는 함수
library(MASS)
date2 <- seq(as.Date('2019/09/28','%Y/%m/%d'), #test할 데이터의 날짜
             as.Date('2019/11/04','%Y/%m/%d'),1)

ts_date <- matrix(rep(0,len=1178),nrow=31,ncol=38) #날짜data포함
ts_date <- as.data.frame(ts_date)
colnames(ts_date) <- date2
#train할 날짜데이터를 가지는 DF
for(i in 1:38){
  b <- c()
  day <- c()
  day <- date2[i]
  b <- date2[i]+7 #예측할 label값(1주뒤)
  b <- c(b,date2[i])
  b <- c(b,day-years(1)) #1년 전 데이터
  b <- c(b, day-seq(1,28,1))
  ts_date[,i] <- b
}
rownames(ts_date) <- c('label','today','1year',1:28)
ts_date <- t(ts_date)

#가격데이터를 담은 DF
ts_price <- matrix(rep(0,len=1178),nrow=38,ncol=31)
ts_price <- as.data.frame(ts_price)
for(i in 1:38){
  for(j in 1:31){
    ts_price[i,j] <- tr[ts_date[i,j]==tr$date,1+2*n]
    #0을 포함하는 행을 제거하기 위해 행번호 수집
    #if(tr[ts_date[i,j]==tr$date,1+2*1]==0){
    #  ts_price[i,j] <- mean(tr[,1+2*1]==0)
    #}
  }
}
subset <- subset(tr,tr[,1+2*n]!=0)
laplace <- mean(subset[,1+2*n])
for(i in 1:38){
  for(j in 1:31){
    #0을 포함하는 행을 제거하기 위해 행번호 수집
    if(ts_price[i,j]==0){
      ts_price[i,j] <- laplace #0을 중간값으로 대체한다
    }
  }
}

colnames(ts_price) <-c('label','today','1year',1:28)
rownames(ts_price) <- date2
View(ts_price)

#ml <- regression(1)#선형 회귀 모델이지만 나는 함수가 안됨
ml <- model
pred <- predict(ml,ts_price[2:31])
ts_price[1]
nm(pred,ts_price[,1])
