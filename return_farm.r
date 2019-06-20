install.packages("readr")
install.packages("dplyr")
install.packages("gmodels")
install.packages("MASS")
install.packages("graphics")
install.packages("ggplot2")
install.packages("psych")
install.packages("ppcor")
install.packages("moonbook")
install.packages("reshape")
install.packages("lsmeans")
library(readr)
library(dplyr)
library(gmodels)
library(MASS)
require(graphics)
require(car)
library(ggplot2)
library(psych)
library(ppcor)
require(moonBook)
library(reshape)
library(lsmeans)
##데이터 불러오기
kosisdata <- read.csv("mean_kosis.csv")
head(kosisdata)
farmer <- read.csv("farmer_av.csv")
data<- merge(kosisdata,farmer,by="loc")
nrow(data)
head(data)
##컬럼 선택 (loc,mean_고령인구비율,mean_순이동인,mean_인구증가율,mean_전입인구,mean_전출인구,mean_합계출산율 제외)
sel.data<-data[,-1]
sel.data<-sel.data[,-3]
colnames(sel.data)
sel.data<-sel.data[,-8]
head(sel.data)
sel.data<-sel.data[,-16]
sel.data<-sel.data[,-c(17:18)]
colnames(sel.data)
sel.data<-sel.data[,-20]
colnames(sel.data)
sel.data<-sel.data[,-c(20:24)]## 년도별 귀농인구수 제외
sel.data<-sel.data[,-8]
sel.data<-sel.data[,-17]
colnames(sel.data)

sel.data <- sel.data%>%
  mutate(value4 = (0.75*mean_X119안전센터.1개센터당.담당주민수 ) + (0.59*mean_EQ.5D지표)+(0.5*mean_소방공무원.1인당.담당주민수))
colnames(sel.data)
sel.data <- sel.data%>%
  mutate(value3 = (0.58*mean_도로포장률 ) + (0.58*mean_상수도보급률)+(0.71*mean_하수도보급률))
sel.data <- sel.data%>%
  mutate(value2 = (0.81*mean_인구.천명당.의료기관병상수 ) + (0.96*mean_인구.천명당.의료기관종사의사수))
sel.data <- sel.data%>%
  mutate(value1 = (0.48*mean_구조.구급대원.1인당.담당주민수 ) - (0.43*mean_노인.천명당.노인여가복지시설수)+(0.44*mean_유아.천명당.보육시설수)+(0.91*mean_유치원수)-(0.52*mean_인구.십만명당.문화기반시설수)+(0.61*mean_인구.천명당.사설학원수)+(0.97*mean_초등학교수))
sel.data1 <- sel.data[,18:22]

fit.red1 <-  lm(value1 ~mean_구조.구급대원.1인당.담당주민수+mean_유아.천명당.보육시설수+mean_유치원수+mean_인구.천명당.사설학원수+mean_초등학교수-mean_노인.천명당.노인여가복지시설수-mean_인구.십만명당.문화기반시설수, data = sel.data)
summary(fit.red1)
fit.red2 <-  lm(value2 ~mean_인구.천명당.의료기관병상수+mean_인구.천명당.의료기관종사의사수, data = sel.data)
summary(fit.red2)
fit.red3 <-  lm(value3 ~mean_도로포장률+mean_상수도보급률+mean_하수도보급률, data = sel.data)
summary(fit.red3)
fit.red4 <-  lm(value4 ~mean_X119안전센터.1개센터당.담당주민수+mean_EQ.5D지표+mean_소방공무원.1인당.담당주민수, data = sel.data)
summary(fit.red4)




##omit으로 NA값 삭제
head(sel.data1)
omit.data<- na.omit(sel.data1)
##전체 다중회귀
fit.red1 <-  lm(avReFarmer ~., data = omit.data)
summary(fit.red1)

##후진제거법
reduced.model=step(fit.red1,direction ="backward" )

summary(reduced.model)

par(mfrow=c(2,2))
plot(fit.red1)


#회귀가정 확인
#install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(reduced.model)
summary(gvmodel)
##다중공선성 확인
#install.packages("car")
library(car)
vif(reduced.model)
sqrt(vif(reduced.model))>2

##후진제거법 후에 나온 변수 전체 회귀
fit.red1 <-  lm(avReFarmer ~mean_도로포장률+mean_상수도보급률+mean_하수도보급률+mean_구조.구급대원.1인당.담당주민수+mean_유아.천명당.보육시설수+mean_유치원수+mean_인구.천명당.사설학원수+mean_초등학교수-mean_노인.천명당.노인여가복지시설수-mean_인구.십만명당.문화기반시설수, data = sel.data)
summary(fit.red1)

##후진제거법 후에 유의미한 변수 회귀
fit.red2 <-  lm(avReFarmer~mean_상수도보급률+mean_유치원수+mean_인구.천명당.사설학원수, data = sel.data)
summary(fit.red2)

#install.packages("leaps")
require(leaps)
leaps <- regsubsets(avReFarmer ~mean_상수도보급률+mean_소방공무원.1인당.담당주민수 +mean_유치원수+mean_인구.천명당.사설학원수, data = sel.data ,nbest = 8)
plot(leaps,scale="adjr2")

##두 모델의 차이 없음
anova(fit.red1,fit.red2)


##최종모델
fit.red2 <-  lm(avReFarmer~mean_상수도보급률+mean_유치원수+mean_인구.천명당.사설학원수, data = sel.data)
summary(fit.red2)



city.data <- kosisdata%>%
  mutate(value3 = (0.58*mean_도로포장률 ) + (0.58*mean_상수도보급률)+(0.71*mean_하수도보급률))
city.data <- city.data%>%
  mutate(value1 = (0.48*mean_구조.구급대원.1인당.담당주민수 ) - (0.43*mean_노인.천명당.노인여가복지시설수)+(0.44*mean_유아.천명당.보육시설수)+(0.91*mean_유치원수)-(0.52*mean_인구.십만명당.문화기반시설수)+(0.61*mean_인구.천명당.사설학원수)+(0.97*mean_초등학교수))

#상위 5개 도시
top.data <- city.data %>% 
  filter(loc == "양평군" | loc == "밀양시" | loc == "창녕군" | loc == "고흥군" |loc == "영천시" )
top <- top.data[,2:28]

top <- colMeans(top,na.rm = TRUE)
top <- data.frame(top)
top <- t(top)

#하위 5개 도시

bot.data <- city.data %>% 
  filter(loc == "고령군" |loc == "철원군" |loc == "울진군" |loc == "삼척시" |loc == "증평군")
bot <- bot.data[,2:28]

bot <- colMeans(bot,na.rm = TRUE)
bot <- data.frame(bot)
bot <- t(bot)

#합천
hap.data <- city.data %>% 
  filter(loc == "합천군" )

hap <- hap.data[,2:28]

hap <- colMeans(hap,na.rm = TRUE)

hap <- data.frame(hap)
hap <- t(hap)

total <- rbind(top,bot)

total <- rbind(total,hap)

