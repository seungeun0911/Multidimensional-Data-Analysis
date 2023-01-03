install.packages("installr")
library(installr)
check.for.updates.R()
install.R()
version

install.packages("knitr")
install.packages("htmlwidgets")
install.packages("ggplot2")
install.packages("extrafont")
install.packages("plotly")
install.packages("ggmap")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")
install.packages("dplyr")
install.packages("raster")
install.packages("leaflet")
install.packages("leafletCN")
install.packages("stringr")
install.packages("DT")
install.packages("cluster")
install.packages("ggthemes")

library(knitr)
library(htmlwidgets)
library(ggplot2)
library(extrafont)
library(plotly)
library(ggmap)
library(rgeos)
library(maptools)
library(rgdal)
library(dplyr)
library(raster)
library(leaflet)
library(leafletCN)
library(stringr)
library(DT)
library(cluster)
library(ggthemes)

getwd()
setwd("C:/Users/smini/Desktop")
cultures <- read.csv("cultures.csv", encoding = "euc-kr")

str(cultures)
datatable(cultures, filter = 'top')

# 주소값이 잘못 들어간 경우
print(cultures$주소[7])

# x, y좌표값이 결측치로 나타난 케이스
print(cultures$X[223])
print(cultures$Y[223])

# 주소 전처리
new_cultures <- read.csv("new_cultures.csv", encoding = "euc-kr")

# API 키 불러오기
install.packages("devtools")
devtools::install_github('dkahle/ggmap')
library(ggmap)
register_google(key='AIzaSyDiyVl4_YRaXXRB35G_zyOGIx72OCCMdt0')

# 주소를 정확한 위경도 값으로 변환
address <- new_cultures$주소
address <- enc2utf8(address)
latlon <- geocode(data = address, address, source = 'google')
print(head(latlon))

# 좌표값 원본 데이터프레임에 cbind
cultures <- cbind(new_cultures, latlon)

# API 키 불러오기
register_google(key = 'AIzaSyDiyVl4_YRaXXRB35G_zyOGIx72OCCMdt0')

# 서울 맵 가져오기
seoul <- get_map("Seoul, South Korea", zoom=11, maptype = "roadmap")

# 좌표 표시하고 업종별로 색 입히기
seoulplot <-  ggmap(seoul) + geom_point(cultures, mapping = aes(x = lon, y = lat, color = 주제분류))
ggplotly(seoulplot)

pal <- colorFactor('Paired', cultures$주제분류) #색을 입힐 factor 변수 지정. 첫번째 인자는 Color palette.

seoul_leaf <- leaflet(cultures) %>% 
  addTiles() %>% 
  setView(lng = 126.97,
          lat = 37.542,
          zoom = 11) %>% 
  addProviderTiles('CartoDB.Positron') %>% #지도 타입 설정
  addCircleMarkers(data = cultures %>%  #필요 데이터 가져오기
                     mutate(pop = paste0('공간명 : ', 문화시설명,
                                         '<br> 분류 : ', 주제분류)), #레이블 값을 지정, dplyr의 mutate 문법 활용
                   popup = ~pop, #새로 만든 변수를 popup시킴
                   lng = ~lon, lat = ~lat, color = ~pal(주제분류), #미리 지정해둔 color pal 가져오기
                   radius = 3) %>% 
  addLegend('bottomright', pal = pal, values = ~주제분류,
            title = '시설 종류', opacity = 1) #legend 추가

seoul_leaf  

# 주소 변수에서 시군구 관련 내용만 추출하여 factor 변수화 시키는 작업
district <- c('강남구','강동구','강북구','강서구','관악구',
              '광진구','구로구','금천구','노원구','도봉구',
              '동대문구','동작구','마포구','서대문구','서초구',
              '성동구','성북구','송파구','양천구','영등포구','용산구',
              '은평구','종로구','중구','중랑구', '양평', '하남') #필요한 구 이름 가져오기

str <- function(string){
  if (T %in% str_detect(string, district) == T) {
    
    a <- which(str_detect(string, district) == T)
    return(district[a])
    
  } else {
    
    return('unknown') #district 변수에 없으면 unknown 반환
    
  }
}# 해당 구가 주소 내용에 포함이 되어 있으면 그 구를 반환


dist_info <- mapply(str, cultures$주소) #mapply로 함수 적용
dist_info <- as.data.frame(dist_info) #데이터프레임화

#unknown 처리된 값들 가져와서 manual하게 처리
loc = which(dist_info$dist_info == 'unknown')
dist_info$dist_info[loc] = c('강남구','고양','군포',
                             '과천','포천','안양',
                             '구리','성남','안산',
                             '안양','의정부','과천',
                             '남양주','부천','구로구',
                             '파주','강남구','성남')

# 원본 데이터에 묶기
cultures <- cbind(cultures, dist_info)
print(head(cultures$dist_info))

s <- ggplot(data = cultures,
            mapping = aes(
              x = dist_info,
              fill = 주제분류
            ))+
  geom_bar(position = 'dodge')+
  theme_fivethirtyeight()+
  theme(axis.text.x   = element_text(angle = 315, size = 5.5, family = 'MapoDPP'),
        plot.title    = element_text(hjust = 0.5, family = 'MapoDPP'),
        legend.text   = element_text(family = 'MapoDPP'),
        legend.title  = element_text(family = 'MapoDPP'))+
  ggtitle('자치구/도시 별 문화시설 분포')

ggplotly(s)

# 위경도 좌표값을 이용한 단순한 산점도
plot(cultures$lat, cultures$lon)

# 일종의 이상치인 값들 제거
cultures <- cultures[cultures$lon < 127.6 & cultures$lat < 37.8, ]

clstdata <- dplyr::select(cultures, lon, lat)
clstdata <- apply(clstdata, 2, scale)                
clstdata <- as.data.frame(clstdata)

# K-means clustering 실행
kmeans_result <- kmeans(clstdata, centers = 6, iter.max = 1000)
clstdata$clst <- as.factor(kmeans_result$cluster)

qplot(lon, lat, color = clst, data = clstdata)     

#cluster 정보 입력된 plot
cultures$clst <- as.factor(kmeans_result$cluster) #클러스터 결과물 변수로 저장  

pal <- colorFactor('Dark2', cultures$clst)

seoul_leaf <- leaflet(cultures) %>% 
  addTiles() %>% 
  setView(lng = 126.97,
          lat = 37.542,
          zoom = 11) %>% 
  addProviderTiles('CartoDB.Positron') %>% 
  addCircleMarkers(data = cultures %>% 
                     mutate(pop = paste0('공간명 : ', 문화시설명,
                                         '<br> 분류 : ', 주제분류,
                                         '<br> n번째 군집 : ', clst)), #군집 정보 추가가
                   popup = ~pop,
                   lng = ~lon, lat = ~lat, color = ~pal(clst),
                   radius = 3) %>% 
  addLegend('bottomright', pal = pal, values = ~clst,
            title = '군집 번호', opacity = 1)

seoul_leaf
