# KDX 소비자트렌드 코리아 2020

## 목표
- KDX의 다양한 데이터와 외부데이터를 활용해 한국의 소비트렌드를 분석해 인사이트를 도출하고 시각화를 하자.

## 설명
 - 인원 : 3명
 - 작업툴 : RStudio

## 데이터
 - 엠코퍼레이션 온라인 구매 데이터

## 프로그래밍

### 1. 라이브러리 import하기
```{r}
library(tidyverse) # 데이터 가공 및 시각화
library(readxl) # 엑셀파일 불러오기 패키지
library(ggplot2) # 그래프 그리기
library(lubridate) # 날짜형식
library(dplyr)
library(zoo)
library(modelr)
options(na.action = na.warn)
library(pastecs) # 독립검정
library(WRS)
library(utils)
```

### 2. 데이터 불러오기 및 통합
 - 우리가 사용할 데이터인 엠코퍼레이션 온라인 구매 데이터를 불러오고 데이터를 통합한다.
```{r}
files <- list.files(path = "data/Mcorporation/상품 카테고리 데이터_KDX 시각화 경진대회 Only", pattern = "*.xlsx", full.names = T)

products <- sapply(files, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id") %>% 
  select(-id)

glimpse(products)
```

### 3. 데이터를 요약하여 살펴보자
 - 고객성별, OS유형별, 나이별 요약
```{r}
#고객성별 요약
products %>%
  filter(고객성별 != "없음") %>%
  select(고객성별, 구매금액, 구매수) %>%
  group_by(고객성별) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균), 수량비율 = 구매수평균 * 100 / sum(구매수평균))

#OS유형별 요약
products %>%
  filter(OS유형 != "없음") %>%
  select(OS유형, 구매금액, 구매수) %>%
  group_by(OS유형) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균), 수량비율 = 구매수평균 * 100 / sum(구매수평균))

#고객나이별 요약
products %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  select(고객나이, 구매금액, 구매수) %>%
  mutate(금액비율 = 구매금액 * 100 / sum(구매금액), 수량비율 = 구매수 * 100 / sum(구매수)) %>%
  group_by(고객나이) %>%
  summarize(구매금액평균 = mean(구매금액), 구매수평균 = mean(구매수), 금액비 = sum(금액비율), 수량비 = sum(수량비율))
```

 - 카테고리별 요약
```{r}
#카테고리별 요약
products %>%
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매금액평균 = mean(구매금액)) %>%
  mutate(금액비율 = 구매금액평균 * 100 / sum(구매금액평균)) %>%
  arrange(desc(구매금액평균)) %>%
  head(10)

products %>%  #구매금액 총합
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

products %>%  #구매수 평균 총합
  select(카테고리명, 구매금액, 구매수) %>%
  group_by(카테고리명) %>%
  summarize(구매수평균 = mean(구매수)) %>%
  summarize(평균합 = sum(구매수평균))
```

### OS별 매달 구매금액 변화 그래프
```{r}
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
  theme_bw() +
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12))
```

- 스마트폰의 보급이 확산됨에 따라 스마트폰을 이용한 쇼핑이 상용화되면서, 상대적으로 안드로이드나 IOS를 통해 구매하는 경향이 점점 커지고 있음을 확인할 수 있다.
 
- 따라서 안드로이드와 IOS의 카테고리별, 나이대별 소비패턴을 파악하고, 이에 따라 OS별 광고를 차별화할 필요가 있다.


### 안드로이드와 IOS의 카테고리별 구매금액 
 - 안드로이드
```{r}
products %>%
  filter(OS유형 == "안드로이드") %>%
  group_by(카테고리명, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  arrange(desc(mean)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(카테고리명, -mean), y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=.5, fill = "#619CFF") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()
```

 - IOS 
```{r}
products %>%
  filter(OS유형 == "IOS") %>%
  group_by(카테고리명, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  arrange(desc(mean)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(카테고리명, -mean), y = mean)) +
  geom_bar(stat="identity", position = "dodge", width=.5, fill = "#fc9272") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6,)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()
```

 - 안드로이드와 IOS의 평균 구매금액 상위 10개의 카테고리를 각각 시각화하였다 .
 - 두 그룹 모두 ‘서비스/티켓’, ‘도서/음반/DVD’, ‘여성의류’가 주로 소비되고 있다.
 - IOS의 경우 안드로이드에 비해 ‘신발’, ‘가방/지갑/잡화’, ‘메이크업 용품’, ‘남성의류’의 소비가 상대적으로 크며, ‘농축수산물’, ‘건강식품’의 소비가 상대적으로 작은 것을 확인할 수 있다.

### 나이대별 남녀 소비 비율
 - 안드로이드
```{r}
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
  theme_bw()
```
 - IOS
```{r}
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
  theme_bw()
```

 - IOS의 경우 상대적으로 여성의 소비 비율이 더 크며, 특히 20, 30대 여성 비율이 압도적으로 큰 것을 확인할 수 있다.
 - 앞에서 ‘가방/지갑/잡화’, ‘메이크업 용품’의 소비가 크게 나타난 이유가 20, 30대 여성 소비가 크기 때문에 알 수 있다

### 안드로이드와 IOS 나이대별 구매금액
```{r}
products %>%
  filter(OS유형 == "안드로이드" | OS유형 == "IOS") %>%
  filter(고객나이 > 0 & 고객나이 < 80) %>%
  group_by(고객나이, OS유형) %>%
  summarise(mean = mean(구매금액)) %>%
  ggplot(aes(x = 고객나이, y = mean, fill = OS유형)) +
  geom_bar(stat="identity", position = "dodge", width=5) + 
  scale_fill_manual(values = c('#F8766D','#619CFF')) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold")) +
  theme_bw()
```

 - IOS : 10대 20대 연령층에서 소비 경향이 크며, 소비하는 금액은 젊은 학생이나 사회 초년생이므로 30,40대 보다 소비하는 금액은 상대적으로 적다.
 - 안드로이드 : 30대 이상 연령층에서 소비 경향이 크며, 안정적인 소득이 있는 직장인들의 소비가 많기 때문에 소비하는 금액이 젊은층보다 상대적으로 크다.
 - 앞에서 안드로이드의 ‘농축수산물’, ‘건강식품’의 소비가 상대적으로 크게 나타나는 이유가 중장년층에서의 소비가 크기 때문임을 알 수 있다.


### OS별 매달 구매금액, 구매수 변화 그래프
 - 구매금액 변화
```{r}
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
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12)) +
  theme_bw()
```

 - 구매수 변화
```{r}
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
  theme(legend.title = element_text(size = 15, face = "bold")) +
  theme(legend.text = element_text(size = 12)) + 
  theme_bw()
```

 - IOS의 경우, WINDOWS에 비해 상대적으로 구매금액 대비 구매 수가 큰 것을 볼 수 있다.이는 저가의 제품을 다량소비하는 패턴이 있음을 말해준다.
 
 - 앞의 결과를 바탕으로 볼 때, IOS를 통한 소비패턴이 학생 및 사회 초년생 층에서 나타나기 때문에, 주로 저가 제품을 다량 소비하는 경향이 있는 것으로 보인다.


### 독립 t검정
```{r}
#데이터 필터링
products1 <- products %>%
  mutate(년월 = as.yearmon(as.character(구매날짜), "%Y%m")) %>%
  rename(date = "년월") %>%
  filter(OS유형 == "IOS" | OS유형 == "안드로이드") %>%
  group_by(date, OS유형) %>%
  summarise(mean = mean(구매금액))
glimpse(products1)
#평균과 표준오차
by(products1$mean, products1$OS유형, stat.desc, basic = FALSE, norm = TRUE)
#t.test()
ind.t.test<-t.test(mean ~ OS유형, data = products1)
ind.t.test
```

 - p 값(p  < 2.2e-16)이 0.05보다 작으므로 귀무가설은 기각되었다. 
 - 평균적으로 안드로이드를 사용한 소비의 구매금액(M = 27,478,580, SE = 334116 )이 IOS를 사용한소비의 구매금액 (M = 14,926,760, SE = 186361)보다 더 크다. 
 - 그 차이는 유의하다(t (26.644) = -32.809, p  < 0.05).

### 분석 결론
 - IOS의 경우
 젊은 층, 특히 젊은 여성층을 공략하는 광고전략이 효과적일 것이며, 고가의 제품보다는 저렴한 제품을 광고로 노출시키는 것이 판매효과를 증진시킬 수 있을 것이다.

 - 안드로이드의 경우
 중장년층을 타겟으로 가격경쟁력적인 측면보다는 소비의 니즈를 만족 시킬 수 있는 좋은 성능을 내세워 광고하는 것이 효과적일 것이다.
