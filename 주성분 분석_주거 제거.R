data<-read.csv("data.csv")

head(data)

#데이터 표준화
func_std<-function(data){
  (data-mean(data))/(sd(data))
}

data2<-sapply(data[,-1],func_std)
data2<-data2[,-3]

#공분산 행렬 이용
pca_c<-prcomp(data2)
summary(pca_c)
pca_c

#상관계수 행렬 이용
pca_r<-prcomp(data2,scale=T)
summary(pca_r)
pca_r

#고유값과 고유벡터
pca_r$sdev
pca_r$rotation[, 1:2]  
pca_r$x[,1:2]

#관측치별 주성분 점수 계산
pc1 <- pca_r$x[, 1]
pc2 <- pca_r$x[, 2]
cor(pc1, pc2) 

data_s <- scale(data2)
summary(data_s)

rot1 <- pca_r$rotation[, 1:2]
rot1
plot(data_s%*%rot1, pca_r$x[, 1:2])

#screeplot
screeplot(pca_r, type = "l")

#행렬도
biplot(pca_r)
text(pc1, pc2, labels = data$행정동, 
     cex = 0.7, pos = 3, col = "blue")
