# KDX 소비자트렌드 코리아 2020

## 목표
- KDX의 다양한 데이터와 외부데이터를 활용해 한국의 소비트렌드를 분석해 인사이트를 도출하고 시각화를 하자.

## 설명
 - 인원 : 3명
 - 작업툴 : RStudio

## 데이터
 - 엠코퍼레이션 온라인 구매 데이터

## 프로그래밍

### 1. 준비하기
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
