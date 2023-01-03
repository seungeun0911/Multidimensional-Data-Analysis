setwd("C:/Users/smini/Desktop")
data<-read.csv("data.csv")

head(data)

#데이터 표준화
func_std<-function(data){
  (data-mean(data))/(sd(data))
}

data2<-sapply(data[,-1],func_std)

#초기 요인분석
install.packages(c("psych","GPArotation"))
library(psych)
library(GPArotation)
library(tidyverse)

data.factor <- principal(data2, rotate="none")
names(data.factor)

data.factor$values

plot(data.factor$values, type="b")

#요인분석
data_factanal <- factanal(data2,
                          factors = 2,
                          rotation="varimax",
                          scores="regression")
print(data_factanal)
print(data_factanal$loadings, cutoff=0)

#biplot
data_factanal$scores
plot(data_factanal$scores, main="Biplot of the first 2 factors")

#관측치별 이름 매핑(rownames mapping)
text(data_factanal$scores[,1], data_factanal$scores[,2], 
     labels = data$행정동, 
     cex = 0.7, pos = 3, col = "blue")

#factor loadings plotting
points(data_factanal$loadings, pch=19, col = "red")

text(data_factanal$loadings[,1], data_factanal$loadings[,2], 
    labels = rownames(data_factanal$loadings), 
    cex = 0.8, pos = 3, col = "red")

#plotting lines between (0,0) and (factor loadings by Var.)
segments(0,0,data_factanal$loadings[1,1], data_factanal$loadings[1,2])
segments(0,0,data_factanal$loadings[2,1], data_factanal$loadings[2,2])
segments(0,0,data_factanal$loadings[3,1], data_factanal$loadings[3,2])
segments(0,0,data_factanal$loadings[4,1], data_factanal$loadings[4,2])
segments(0,0,data_factanal$loadings[5,1], data_factanal$loadings[5,2])
segments(0,0,data_factanal$loadings[6,1], data_factanal$loadings[6,2])

fa.diagram(data_factanal$loadings)

################################################################################
#최우추정법
stats.fact <- factanal(data2, factors = 2, rotation="oblimin")
stats.fact
################################################################################

#요인별 변수 묶기
fa1<-data.frame(data$업무, data$숙박, data$유동인구, data$상업, data$문화)
fa2<-data.frame(data$주거)

fa1<-round((fa1$data.업무+fa1$data.숙박+fa1$data.유동인구+fa1$data.상업+fa1$data.문화)/ncol(fa1),5)
fa2<-round((fa2$data.주거)/ncol(fa2),1)

subject_factor_df<-data.frame(fa1, fa2)
cor(subject_factor_df)