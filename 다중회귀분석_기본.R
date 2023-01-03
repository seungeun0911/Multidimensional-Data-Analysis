setwd("C:/Users/smini/Desktop")
cultures_data<-read.csv("cultures_data.csv")

문화시설<-cultures_data$문화시설.수
cultures_data<-cbind(data,문화시설)
cultures_data<-cultures_data[,-4]

head(cultures_data)
summary(cultures_data)

library(psych)
pairs.panels(cultures_data)

model<-lm(문화시설~업무+상업+숙박+문화+유동인구,data=cultures_data)
summary(model)

#추정된 회귀식
#문화시설=(-1.693e+00)+(2.246e-01)*업무+(-1.847e-01)*상업
#        +(2.621e-02)*숙박+(1.769e-01)*문화+(2.064e-05)*유동인구
#R-squared: 0.9908

plot(model)

library(car)
vif(model)
#업무와 숙박의 VIF가 10을 넘어 다중공선성이 있음을 확인