# EKAPE Parsing Script for GitHub Actions
# 서비스 계정을 사용한 Google Sheets 인증

# Load libraries
library(tidyverse)
library(readr)
library(stringr)
library(rvest)
library(lubridate)
library(googlesheets4)
library(gargle)

# ============================================
# Google Sheets 인증 (서비스 계정 사용)
# ============================================
gs4_auth(path = "service-account.json")

# ============================================
# 데이터 크롤링
# ============================================
dd <- rvest::read_html("https://www.ekapepia.com/supPrice/liveStock/type/cowNew.do?menuSn=86&boardInfoNo=&bbsSn=&userGroupType=40&paramSearchTxt=&pstSn=0") %>%
  html_table() %>%
  .[[1]] %>%
  janitor::clean_names(ascii = FALSE) |>
  dplyr::slice(-c(1:2))

names(dd) <- c("date", "암송아지", "숫송아지", "농가수취가격_600kg", "지육_평균", "지육_1등급", "도매_등심1등급", "소비자_등심1등급")

dd1 <- dd %>%
  mutate_at(
    vars(암송아지:소비자_등심1등급), function(x) {parse_number(x)}
  ) %>%
  mutate_at(
    vars(암송아지:농가수취가격_600kg),
    function(x) x*1000
  ) %>%
  mutate(명절 = NA) %>%
  select(date, 명절, everything())

# ============================================
# Google Sheets 읽기 & 쓰기
# ============================================
sheet_url <- "https://docs.google.com/spreadsheets/d/1baViBjPUz1wyicG-I-vN5RhyoCcFt-6lw550evTowEg/edit#gid=1181990123"

df <- read_sheet(sheet_url, sheet = 1)

df <- df %>% 
  select(-wday) %>% 
  unique() %>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  select(date:week, wday, 비고)

df1 <- dd1 %>%
  mutate(date = ymd(date),
         year = year(date),
         week = isoweek(date),
         wday = wday(date, label = TRUE)
  )

df2 <- df %>%
  mutate(date = ymd(date)) %>%
  filter(date %in% df1$date == FALSE)

df_final <- bind_rows(df1, df2)

# 중복 확인
dupes <- janitor::get_dupes(df_final)
if(nrow(dupes) > 0) {
  message("중복 데이터 발견:")
  print(dupes)
}

glimpse(df_final)

# Google Sheets에 쓰기
write_sheet(data = df_final, sheet_url, sheet = 1)

# 로컬 파일로 저장
dir.create("data_hanwoo_stock", showWarnings = FALSE)
rio::export(df_final, file = paste0("data_hanwoo_stock/hanwoo-stock-", today(), ".xlsx"))

message("✅ 데이터 업데이트 완료: ", today())
