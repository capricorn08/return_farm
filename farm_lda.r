options(java.parameters=c("-Xmx12g"))
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_211')

library(rJava)
library(KoNLP)
library(tidyverse)
library(tibble)
library(topicmodels)
library(tidytext)
library(broom)
library(rJava)
library(arules) 
library(igraph) 
library(combinat) 
library(tm) 
library(proxy) 
library(stringr)
library(readxl)
library(dplyr)
library(RMySQL)
library(readr)
library(slam)
library(xlsx)
library(xlsxjars)
useSejongDic() 
useNIADic()


#hap_c <-read.csv("./hapcheon/hap_1418_1.csv",sep = ";", h = T, fileEncoding = "UTF-8")
hap_c <- read.xlsx("./hapcheon/hap_1418.xlsx",sheetIndex = 1, fileEncoding = "euc-kr")
hap_c <-read.csv("./hapcheon/news_trasfer_suburban.CSV")
hap_2016 <-read.csv("./hapcheon/2016.csv", h = T, fileEncoding = "euc-kr")
hap_1618 <-read.csv("./hapcheon/SUCCESS.csv", h = T)

nong_1418 <-read.csv("./hapcheon/nong_1418.csv", h = T) ## notepad utf8로 변환 저장



dataDB <- hap_c[,-c(2:16)]
dataDB <- nong_1418[,-c(2:16)]



names(dataDB)[1] = c("id")
names(dataDB)[2] = c("contents")

colnames(dataDB)

dataDB <- hap_1618

dataDB$id <- as.factor(dataDB$id)
dataDB$contents <- as.character(dataDB$contents)

colnames(dataDB)
dataDB_b <- dataDB

dataDB <- hap_2016
datapus <- dataDB$contents
datapus <- gsub("[a-zA-Z]"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("\\d+"," " , datapus ,ignore.case = TRUE)
datapus <- gsub("[^가-힣]", " ", datapus ,ignore.case = TRUE)
datapus <- gsub('[~!@#$%&*()_+=?<>■▶]',' ',datapus ,ignore.case = TRUE)
datapus <- gsub("가운데"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("대표"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("지난해"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("올해"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("기자"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("오전"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("이날"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("오후"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("이번"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("개최"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("도내"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("시작"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("기준"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("이상"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("사업"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("선정"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("으로"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("의원"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("합천군"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("합천"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("선거구"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("하창환"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("경남도"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("경남지역"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("경남"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("지난달"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("새누리당"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("누리당"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("대표"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("국회의원"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("의원"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("다양한"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("이상"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("관심"," " ,datapus ,ignore.case = TRUE)
#datapus <- gsub(""," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("귀농"," ", datapus ,ignore.case = TRUE)
datapus <- gsub("귀촌"," ", datapus ,ignore.case = TRUE)
datapus <- gsub("귀농귀촌"," ", datapus ,ignore.case = TRUE)
datapus <- gsub("충청일보"," ", datapus ,ignore.case = TRUE)
datapus <- gsub("지난"," ", datapus ,ignore.case = TRUE)
datapus <- gsub("안정적"," ", datapus ,ignore.case = TRUE)
datapus <- gsub("중도일보"," ", datapus ,ignore.case = TRUE)
datapus <- gsub("충북도"," ", datapus ,ignore.case = TRUE)

####################################################################################################################3

datapus <- gsub("중부매일"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("아시아경제"," " ,datapus ,ignore.case = TRUE)
datapus <- gsub("충북일보"," " ,datapus ,ignore.case = TRUE)
# datapus <- gsub(""," " ,datapus ,ignore.case = TRUE)

datapus <- gsub("   ","", datapus ,ignore.case = TRUE)
datapus <- gsub("  ","", datapus ,ignore.case = TRUE)

datapus[1:10]

########################################################################
######################불용어 사전#####################################################
sword <- readLines(".//topic//stopword.txt", encoding="UTF-8")
sword <- unique(sword) # 중복제거
sword <- gsub("\uFEFF", "", sword)


sword <- paste(sword, collapse = "|")

#yahoopus_s[[1]]$content
##################################################################################3
# 주제에 맞는 사용자 사전 입력
##################################################################################33333333

newWord <- readLines(".//topic//conword.txt", encoding="UTF-8")
newWord <- gsub("<U+FEFF>", "", newWord)

newDic <- data.frame(newWord, "ncn")
buildDictionary(ext_dic = c("sejong","woorimalssm","insighter"), user_dic=newDic,replace_usr_dic = T)


for(i in seq_along(sword)) {
  datapus <- stringr::str_replace_all(datapus, fixed(sword)," ") 
}


#########################################################################################
#전처리 끝. stopword, conword는 주제에 맞게..
######################################################################################

#########################################################################################
#########################################################################################
ko.words <- function(doc){
  d <- as.character(doc)
  pos <- paste(SimplePos22(d))
  extracted <- str_match(pos, "([가-힣a-zA-Z]+)/(NC)")#명사 가져옴
  
  keyword <- extracted[,2]
  keyword[!is.na(keyword)]
} ## 명사추출함수

options(mc.cores=1)
datapus_s <- VCorpus(VectorSource(datapus))#형태소 분석이 된 데이터를 corpus로 만든다음, tdm 작성

## tdm변환
tdm <- TermDocumentMatrix(datapus_s,
                          control=list(tokenize=ko.words,
                                       removePunctuation=T,
                                       removeNumbers=T,
                                       wordLengths=c(2, 10))) 

View(tdm)
nTerms(tdm)
####################################################################################################
####################################################################################################

data_tdm <- tdm
data_tdm <- removeSparseTerms(data_tdm, sparse = 0.99)

#inspect(yahoo_tdm)

# nTerms(yahoo_tdm)
# nDocs(yahoo_tdm)
# Terms(yahoo_tdm)
# yahoo_tdm <- removeSparseTerms(yahoo_tdm, sparse = 0.99)
# wordFreq <- slam::row_sums(yahoo_tdm)
# wordFreq <- sort(wordFreq,decreasing=TRUE)
# wordFreq
# wordFreq_top <- head(sort(wordFreq,decreasing=TRUE),400)

# 워드클라우드
#######################################################################################33
# wordcloud(names(wordFreq_top), wordFreq_top, scale=c(9,1),random.order = FALSE,
#           rot.per=0.1,min.freq=5, 
#           random.color = TRUE, colors = color,max.words = 200, family = "font")
##########################################################################################

dtm <- as.DocumentTermMatrix(data_tdm)  ## dtm변환
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]    ## 빈도 1이상만 포함
dtm <- dtm.new
inspect(dtm)
View(inspect(dtm))
# term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
# boxplot(term_tfidf)
# term_tfidf

library(topicmodels)
## lda실시 k는 토픽갯수
lda <- LDA(dtm, k=3,control=list(seed=12345))

# term <- terms(lda, 20) #토픽별로 핵심단어20개 추출
terms <- as.data.frame(terms(lda, 20), stringsAsFactors=FALSE)
terms

summary(lda)


###########################그래프 찍기#############################
x <- posterior(lda)$terms # $terms와 $topics를 인덱싱할 수 있음
y <- data.frame(t(x[,apply(x,2,max)>0.03]))
z <- data.frame(type=paste("Topic", 1),
                keyword=rownames(y), posterior=y[,1])

## 토픽번호저장
for (i in 2:5) {
  z <- rbind(z, data.frame(type=paste("Topic",i),
                           keyword=rownames(y), posterior=y[,i]))
  
}

ggplot(z,aes(keyword,posterior, fill=as.factor(keyword)))+
  geom_bar(position = "dodge", stat = "identity")+
  coord_flip()+
  facet_wrap(~type,nrow = 1) +
  theme(legend.position = "none")

#######################################################################################3
##########################################################################################
### 군집분석
##################################################################
###################################################################
dev.new(width = 1000, height = 1000, unit = "px")
w1 <- names(wordFreq[1:30])
data_Cluste <- data_tdm[Terms(data_tdm) %in% w1,]
distmatrix <- dist(scale(data_Cluste))

distmatrix
yahoo_fit <- hclust(distmatrix, method = "ward.D")
plot(yahoo_fit, xlab = "", sub = "", main = "clustering keywords")
rect.hclust(yahoo_fit, k=5)
str(yahooCluste)


##########################################################################
###  textmining package "tidytext"의 활용 
###########################################################################

ap_td <- tidy(dtm)

ap_td

ap_td <- ap_td %>%
  count(document, term, sort = TRUE)%>%
  ungroup()
ap_td

total_words <- ap_td %>%
  group_by(document) %>%
  summarize(total=sum(n))

total_words

at_td <- left_join(ap_td, total_words, by="document")
at_td

freq_by_rank <-at_td %>% 
  group_by(document) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

at_td <- at_td %>%
  bind_tf_idf(document, term, n)
at_td

at_td_tf_idf <- at_td %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

at_td_tf_idf
#TF-IDF(Term Frequency - Inverse Document Frequency)


at_td_tf_idf100 <- head(at_td, by = "tf_idf",100)
View(at_td_tf_idf100)


############################################################################
############################################################################
#LDA beta, gamma 구하기
####################################################################
## gamma 토픽자체가 문서에서 나올확율
## beta 단어가 토픽에서 나올 확률
############################################################################
############################################################################
############################################################################
# remove.packages ("tidytext")
# remove.packages ("topicmodels")
# remove.packages ("broom")
# install.packages("tidytext")
# install.packages("topicmodels")
# install.packages("broom")
# library(tidytext)
# library(topicmodels)
# library(broom)
# library(dplyr)
# library(ggplot2)


hap_topics <- tidy(lda, matrix = "beta") #per-topic-per-word probability
hap_topics



hap_topics_top <- hap_topics %>% 
  group_by(topic) %>% 
  top_n(15, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

hap_topics_top

hap_topics_top %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


## topic1 추출
hap_topics1 <- hap_topics%>%
  filter(topic == 1)%>%
  arrange(desc(beta))

hap_topics1
View(hap_topics1)


head(hap_topics1, by = "beta",100)




library(tidyr)

### topic 간에 beta값 비교
beta_spread <- data_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001 | topic3 > .001 | topic4 > .001| topic5 > .001) %>%
  mutate(log_ratio12 = log2(topic2 / topic1)) 

beta_spread


#####################################################################

#Document-topic probabilities

data_documents_g <- tidy(lda, matrix = "gamma") ## gamma값
data_documents_b <- tidy(lda, matrix = "beta")  ## beta값

data_documents_g
data_documents_b

data_documents_bb <- head(data_documents_b, by = "beta",100)
#해당 topic에서 생성 된 해당 document의 word=term의 예상 비율
# # A tibble: 304,260 x 3
# document topic  gamma
# <chr>    <int>  <dbl>
# 1 1            1 0.0508
# 2 3            1 0.0517 
# 3 10           1 0.0517
# 4 11           1 0.0517
# 5 12           1 0.0494
# 6 13           1 0.0517
# 7 15           1 0.0517
# 8 17           1 0.0508
# 9 19           1 0.0517
# 10 20           1 0.0498
#맨위의 1document의 각 단어는 topic1에서 오는 확률이 5.08%이다.
#

###문서6의 beta값 
data_documents_b6 <- tidy(dtm) %>%
  filter(document == 6) %>%
  arrange(desc(count))

##########################################################
# A tibble: 240 x 3
# topic term          beta
# <int> <chr>        <dbl>
#   1     1 가이드 0.00551    
# 2     2 가이드 0.000000615
# 3     3 가이드 0.00134    
# 4     4 가이드 0.136      
# 5     5 가이드 0.246      
# 6     6 가이드 0.00131    
# 7     7 가이드 0.000166   
# 8     8 가이드 0.00311    
# 9     9 가이드 0.000351   
# 10    10 가이드 0.000883  
#
#해당 TOPic에서 생성되는 term=word의 확률을 계산, 5번째topic에서 생성될 확률24.6%로 가장높음
data_topic1 <- data_documents_b%>%
  filter(topic == 6)

data_topic1

data_topic1_b <- head(data_topic1, by = "beta",100)
data_topic1_b
