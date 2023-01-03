setwd("C:/Users/smini/Desktop")
data<-read.csv("data.csv")

head(data)
pairs.panels(data[,-1])

#산점도
plot(data[,-1])

#상관계수
cor(data[,-1],use='complete.obs',method='pearson')
x<-round(cor(data[,-1],use='complete.obs',method='pearson'),digits=3)

library(corrplot)
corrplot(x)

#상관계수 검정
cor.test(data$업무,data$상업)
cor.test(data$업무,data$주거)
cor.test(data$업무,data$숙박)
cor.test(data$업무,data$문화)
cor.test(data$업무,data$유동인구)
cor.test(data$상업,data$주거)
cor.test(data$상업,data$숙박)
cor.test(data$상업,data$문화)
cor.test(data$상업,data$유동인구)
cor.test(data$주거,data$숙박)
cor.test(data$주거,data$문화)
cor.test(data$주거,data$유동인구)
cor.test(data$숙박,data$문화)
cor.test(data$숙박,data$유동인구)
cor.test(data$문화,data$유동인구)