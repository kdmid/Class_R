library(tidyverse) # 데이터 가공 및 시각화

library(readxl) # 엑셀파일 불러오기 패키지 

sessionInfo()

readr::guess_encoding("data/Samsungcard.csv", n_max = 100)

samsung_card <- read_xlsx("data/Samsungcard.xlsx")

samsung_card2 <- read.csv("data/Samsungcard.csv", fileEncoding = "EUC-KR")

head(samsung_card)

head(samsung_card2)

rm(samsung_card2) # 객체 지우는 함수

ls() # 현재 저장된 객체 확인하는 함수

shinhancard <- read_xlsx("data/Shinhancard.xlsx")

head(shinhancard)

shinhancard <- shinhancard %>% 
  select(-c(6:8))

head(shinhancard)

gin_8a <- read_csv("data/GIN00008A.csv")

gin_9a <- read_csv("data/GIN00009A.csv")

glimpse(gin_8a)

glimpse(gin_9a)

library(jsonlite)

GIN_10m <- fromJSON("data/center_GIN00010M.json")

glimpse(GIN_10m)

readr::guess_encoding("data/Mcorporation/KDX시각화경진대회_SSC_DATA.csv")

ssc_data <- read_csv("data/Mcorporation/KDX시각화경진대회_SSC_DATA.csv", locale = locale("ko", encoding = "EUC-KR"))

glimpse(ssc_data)

#파일 합치기
library(tidyverse) # 데이터 가공 및 시각화

library(readxl) # 엑셀파일 불러오기 패키지 

files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only", pattern = "*.xlsx", full.names = T)

products <- sapply(files, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>% 
  select(-id)

glimpse(products)

# 엠코퍼레이션 코로나 전후로 데이터 나누기
files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only/", pattern = "*.xlsx", full.names = TRUE) # 다중 엑셀파일 불러오기

glimpse(files)

# KDX_CONTEST_파일정의서.xlsx : 파일 제외
products <- sapply(files[2:65], read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>% 
  select(-id)

glimpse(products)




#기술적 통계량
##고객성별 요약
library(ggplot2)
library(lubridate)
library(dplyr)
library(zoo)

products %>%
  filter(고객성별 != "없음") %>%
  select(고객성별, 구매금액, 구매수) %>%
  group_by(고객성별) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균), 수량비율 = 구매수평균 * 100 / sum(구매수평균))

##OS유형 요약
products %>%
  filter(OS유형 != "없음") %>%
  select(OS유형, 구매금액, 구매수) %>%
  group_by(OS유형) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균), 수량비율 = 구매수평균 * 100 / sum(구매수평균))

##고객나이 요약
products %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  select(고객나이, 구매금액, 구매수) %>%
  mutate(금액비율 = 구매금액 * 100 / sum(구매금액), 수량비율 = 구매수 * 100 / sum(구매수)) %>%
  group_by(고객나이) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수), 금액비 = sum(금액비율), 수량비 = sum(수량비율))

##카테고리 요약
products %>%
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매금액평균 = mean(구매금액)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균)) %>%
  arrange(desc(구매금액평균)) %>%
  head(10)

products %>%
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매금액평균 = mean(구매금액)) %>%
  summarize(평균합 = sum(구매금액평균))


products %>%
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매수평균 = mean(구매수)) %>%
  mutate(금매수비율 = 구매수평균 * 100 / sum(구매수평균)) %>%
  arrange(desc(구매수평균)) %>%
  head(10)

products %>%
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매수평균 = mean(구매수)) %>%
  summarize(평균합 = sum(구매수평균))


#OS별 매달 구매금액 변화 그래프
library(ggplot2)
library(lubridate)
library(dplyr)
library(zoo)
products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = mean, colour = OS유형),lwd = 2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="OS 유형별 월별 소비패턴 동향", 
       y="Avg. Sales (1000)") +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))


#OS별 매달 구매금액 추세
library(modelr)
options(na.action = na.warn)

products2 <- products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액))

products_line <- products2 %>% 
  data_grid(date, OS유형) %>%
  gather_predictions(lm(mean ~ date * OS유형, data = products2))

products2 %>%
  ggplot(aes(date, mean, colour = OS유형)) +
  geom_line(lwd = 2) +
  geom_line(data = products_line, aes(y = pred),lwd = 1) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="OS 유형별 월별 소비패턴 동향", 
       y="Avg. Sales (1000)") +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))

#안드로이드의 카테고리별 구매금액
products %>%
  filter(OS유형 == "안드로이드") %>%
  group_by(카테고리명, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  arrange(desc(mean)) %>%
  head(10) %>%
  ggplot(aes(x = 카테고리명, y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=.5, fill = "steelblue") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="안드로이드", 
       subtitle="카테고리명 Vs Avg. 구매금액 by OS유형", 
       caption="source: products")

#IOS의 카테고리별 구매금액
products %>%
  filter(OS유형 == "IOS") %>%
  group_by(카테고리명, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  arrange(desc(mean)) %>%
  head(10) %>%
  ggplot(aes(x = 카테고리명, y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=.5, fill = "#fc9272") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="IOS", 
       subtitle="카테고리명 Vs Avg. 구매금액 by OS유형", 
       caption="source: products")

#안드로이드의 나이대별 구매금액
products %>%
  filter(OS유형 == "안드로이드" & 고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=5, fill = "steelblue") + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="Android", 
       subtitle="카테고리명 Vs Avg. 구매금액 by OS유형", 
       caption="source: products")


#IOS의 나이대별 구매금액
products %>%
  filter(OS유형 == "IOS" & 고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=5, fill = "#fc9272") + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="IOS", 
       subtitle="카테고리명 Vs Avg. 구매금액 by OS유형", 
       caption="source: products")

#안드로이드와 IOS 나이대별 구매금액
products %>%
  filter(OS유형 == "안드로이드" | OS유형 == "IOS") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = OS유형)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="Android", 
       subtitle="카테고리명 Vs Avg. 구매금액 by OS유형", 
       caption="source: products")

#안드로이드와 IOS 나이대별 구매수
products %>%
  filter(OS유형 == "안드로이드" | OS유형 == "IOS") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, OS유형) %>%
  summarise(mean = mean(구매수)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = OS유형)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="Android", 
       subtitle="카테고리명 Vs Avg. 구매수 by OS유형", 
       caption="source: products")

#안드로이드와 IOS 성별 구매금액
products %>%
  filter(OS유형 == "안드로이드" | OS유형 == "IOS") %>%
  filter(고객성별 != "없음") %>%
  group_by(고객성별, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = OS유형, y = mean, fill = 고객성별)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

products %>%
  filter(OS유형 == "안드로이드") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, 고객성별) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = 고객성별)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="Android")

products %>%
  filter(OS유형 == "IOS") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, 고객성별) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = 고객성별)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="IOS")

#통계연습(독립 t검정)
library(ggplot2)
library(pastecs)
library(WRS)
library(utils)
library(dplyr)

##필터링
products1 <- products %>%
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 == "IOS" | OS유형 == "안드로이드") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액))

glimpse(products1)

##독립 t검정
###표준오차 그래프
boxplot(products1$mean ~ products1$OS유형)

ggplot(products1, aes(OS유형, mean)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black") + 
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +
  labs(x = "Type of Stimulus", y = "Anxiety")

by(products1$mean, products1$OS유형, stat.desc, basic = FALSE, norm = TRUE)
#### IOS의 평균은 1.492676e+07이고, 표준편차는 7.906628e+05, 표준오차는 1.863610e+05이다.
####안드로이드의 평균은 2.747858e+07이고 표준편차는 1.417534e+06, 표준오차는 3.341160e+05이다.

ind.t.test<-t.test(mean ~ OS유형, data = products1)

ind.t.test


#OS별 매달 구매금액 변화 그래프
library(ggplot2)
library(lubridate)
library(dplyr)
library(zoo)

products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = mean, colour = OS유형),lwd = 2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="OS 유형별 월별 소비패턴 동향", 
       y="Avg. Sales (1000)") +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))

#OS별 매달 구매금액 추세
library(modelr)
options(na.action = na.warn)

products2 <- products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액))

products_line <- products2 %>% 
  data_grid(date, OS유형) %>%
  gather_predictions(lm(mean ~ date * OS유형, data = products2))

products2 %>%
  ggplot(aes(date, mean, colour = OS유형)) +
  geom_line(lwd = 2) +
  geom_line(data = products_line, aes(y = pred),lwd = 1) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="OS 유형별 월별 소비패턴 동향", 
       y="Avg. Sales (1000)") +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))


#
products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m"), 구매금액_만원 = 구매금액/0.5e5) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean1 = mean(구매금액_만원), mean2 = mean(구매수)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = mean1, colour = OS유형),lwd = 2) + 
  geom_line(aes(y = mean2, colour = OS유형),lwd = 2,linetype = 6) +
  scale_fill_manual(values = c('lightred','lightgreen', 'purple')) +
  scale_y_continuous(sec.axis = sec_axis(~./0.5, name = "Effect (PD)")) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="OS 유형별 월별 소비패턴 동향", 
       y="Avg. Sales (1000)") +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))


#
products %>% 
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 != "없음") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매수)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = mean, colour = OS유형),lwd = 2) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  labs(title="OS 유형별 월별 소비패턴 동향", 
       y="Avg. Sales (1000)") +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))
