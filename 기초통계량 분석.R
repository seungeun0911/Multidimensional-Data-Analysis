setwd("C:/Users/smini/Desktop")
data<-read.csv("data.csv")

#데이터 구조
str(data)

#요약통계량
summary(data)

#각 변수들의 평균, 중위수, 분위수
list<-c("업무","상업","주거","숙박","문화","유동인구")
colMeans(data[list])
apply(data[list],2,median)
apply(data[list],2,quantile)

#각 변수들의 분산 - 공분산
cov(data[list])

#상관분석
cor(data[list])

#각 변수별 산점도
par(mfrow=c(3,2))
scatter.smooth(data$업무)
scatter.smooth(data$상업)
scatter.smooth(data$주거)
scatter.smooth(data$숙박)
scatter.smooth(data$문화)
scatter.smooth(data$유동인구)

#각 변수별 상자그림
boxplot(data$업무, main = "업무")
boxplot(data$상업, main = "상업")
boxplot(data$주거, main = "주거")
boxplot(data$숙박, main = "숙박")
boxplot(data$문화, main = "문화")
boxplot(data$유동인구, main = "유동인구")

#각 변수별 밀도플롯
plot(density(data$업무), main = "업무")
plot(density(data$상업), main = "상업")
plot(density(data$주거), main = "주거")
plot(density(data$숙박), main = "숙박")
plot(density(data$문화), main = "문화")
plot(density(data$유동인구), main = "유동인구")

#왜도
library(e1071)
skewness(data$업무)
skewness(data$상업)
skewness(data$주거)
skewness(data$숙박)
skewness(data$문화)
skewness(data$유동인구)