cultures_data<-read.csv("cultures_data.csv")
cultures_data<-cbind(cultures_data,fa1,fa2)

head(cultures_data)
summary(cultures_data)

library(psych)
pairs.panels(cultures_data)

model<-lm(문화시설~fa1+fa2,data=cultures_data)
anova(model)
summary(model)
#회귀식: 문화시설=(1.510e+00)+(-4.842e-02)*fa1+(2.703e-04)*fa2+𝜖

coefficients(model)
cor.test(cultures_data$fa1, cultures_data$fa2)

library(forecast)
cultures_data
str(cultures_data)

model01<-lm(문화시설~.,data=cultures_data)
model02<-lm(문화시설~fa1,data=cultures_data)
model03<-lm(문화시설~fa2,data=cultures_data)

plot(cultures_data$문화시설.수)
lines(model01$fitted.values,col="black")
lines(model02$fitted.values,col="red")
lines(model03$fitted.values,col="blue")

forecast::accuracy(model01)
forecast::accuracy(model02)
forecast::accuracy(model03)

#fa1, fa2의 기여도
relweights <-
  function(fit,...){                         
    R <- cor(fit$model)   
    nvar <- ncol(R)          
    rxx <- R[2:nvar, 2:nvar] 
    rxy <- R[2:nvar, 1]      
    svd <- eigen(rxx)        
    evec <- svd$vectors                           
    ev <- svd$values         
    delta <- diag(sqrt(ev))  
    lambda <- evec %*% delta %*% t(evec)        
    lambdasq <- lambda ^ 2   
    beta <- solve(lambda) %*% rxy           
    rsquare <- colSums(beta ^ 2)                   
    rawwgt <- lambdasq %*% beta ^ 2    
    import <- (rawwgt / rsquare) * 100 
    lbls <- names(fit$model[2:nvar])   
    rownames(import) <- lbls
    colnames(import) <- "Weights"
    barplot(t(import),names.arg=lbls,
            ylab="% of R-Square",
            xlab="Predictor Variables",
            main="Relative Importance of Predictor Variables", 
            sub=paste("R-Square=", round(rsquare, digits=3)),
            ...)  
    return(import)
  }

result=relweights(model,col="blue")
result

#############################################################################################
pre <- predict(model, newdata = cultures_data)
pre <- as.data.frame(pre)
head(pre)

pre <- predict(model, newdata = cultures_data, interval = "predict")
pre <- as.data.frame(pre)
head(pre)

pre<-cbind(pre,cultures_data$문화시설.수)
head(pre)

tf <- NA
pre <- cbind(pre, tf)

pre$tf[pre$`cultures_data$문화시설.수`>= pre$lwr & pre$`cultures_data$문화시설.수` <= pre$upr] <- T
pre$tf[is.na(pre$tf)] <- F

head(pre)
sum(pre$tf=="TRUE")/dim(pre)[1]

#############################################################################################
#전진선택법
step(model,direction = "forward")
summary(step(model,direction = "forward"))

#후진제거법
step(model,direction = "backward")
summary(step(model,direction = "backward"))
#stepwise
step(model,direction = "both")
summary(step(model,direction = "both"))
#############################################################################################
par(mfrow=c(2,2))
plot(model)
par(mfrow=c(1,1))
#############################################################################################
#정규성 검정
qqPlot(model,labels=row.names(문화시설),id.method="identify",simulate=TRUE,main="Q-Q_c plot")

#오차
residplot <- function(model, nbreaks=10) {
  z <- rstudent(model)
  hist(z, breaks=nbreaks, freq=FALSE,xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,col="red", lwd=2, lty=2)
  legend("topright",legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}
residplot(model)

#독립성
durbinWatsonTest(model)

#선형성
crPlots(model)

#등분산성
ncvTest(model)
spreadLevelPlot(model)

#선형모형 가정에 대한 전반적 검증
install.packages("gvlma")
library(gvlma)
gvmodel<-gvlma(model)
summary(gvmodel)

#다중공선성
vif(model)
sqrt(vif(model))>2 # 다중공선성 문제 없음

#이상치
car::outlierTest(model) 

#큰지레점
hat.plot <- function(model) {
  p <- length(coefficients(model))
  n <- length(fitted(model))
  
  y=hatvalues(model)
  
  name=attr(hatvalues(model),"names")
  df=data.frame(x=1:length(y),y=as.numeric(y),name=name)
 
  require(ggplot2)
  require(ggiraph)
  require(moonBook2)
  p1<-ggplot(df,aes(x=x,y=y,tooltip=name,data_id=x))+geom_point_interactive()
  yintercept2=2*p/n
  p1<-p1+geom_hline(aes(yintercept=yintercept2),col="red",lty="dashed")
  yintercept3=3*p/n
  p1<-p1+geom_hline(aes(yintercept=yintercept3),col="red",lty="dashed")
  ggiraph(code=print(p1))
  
}                 
hat.plot(model)

#영향관측치
car::avPlots(model,ask=FALSE,id.method="identify")

#변환
car::ncvTest(model)
car::spreadLevelPlot(model)
