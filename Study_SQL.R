# 패키지 설치
#install.packages("bigrquery")

library(DBI)
library(bigrquery)
library(dplyr)

#### Sample ####
# 프로젝트 ID
# 각자 고유의 프로젝트 ID 선정
billing <- "bigquerytutorial-274406"

# DB연동
con <- dbConnect(
  bigrquery::bigquery(),
  project = "publicdata",
  dataset = "samples",
  billing = billing
)

# 쿼리문 작성
sql <- "SELECT year, month, day, weight_pounds FROM `publicdata.samples.natality`"
## 선택에서 지메일 계정 확인 1입력


# 프로젝트 내 DB접근
tb <- bq_project_query(billing, sql)

# Data 불러오기
bq_table_download(tb, max_results = 10)

#분석
glimpse(data)

#### 데이터 불러오기 연동 ####
sql_house <- "SELECT * FROM `bigquerytutorial-274406.house_price.train`"
house_tb <- bq_project_query(billing, sql_house)
train <- bq_table_download(house_tb)
glimpse(train)

#### 정형 데이터를 빅쿼리에 저장하기 ####
con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquerytutorial-274406",
  dataset = "house_price",
  billing = billing
)
con
DBI::dbListTables(con)

# mtcars 데이터를 빅쿼리에 저장하기
DBI::dbWriteTable(con, "mtcars", mtcars)