# EKAPE Parsing Script for GitHub Actions
# 서비스 계정을 사용한 Google Sheets 인증
# 최소 패키지 버전

# Load libraries (최소화)
library(dplyr)
library(rvest)
library(googlesheets4)

# ============================================
# Google Sheets 인증 (서비스 계정 사용)
# ============================================
gs4_auth(path = "service-account.json")

# ============================================
# 헬퍼 함수
# ============================================
parse_number_simple <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}

# ============================================
# 데이터 크롤링
# ============================================
dd <- rvest::read_html("https://www.ekapepia.com/supPrice/liveStock/type/cowNew.do?menuSn=86&boardInfoNo=&bbsSn=&userGroupType=40&paramSearchTxt=&pstSn=0") |>
  html_table()

dd <- dd[[1]]
dd <- dd[-c(1:2), ]

names(dd) <- c("date", "암송아지", "숫송아지", "농가수취가격_600kg", "지육_평균", "지육_1등급", "도매_등심1등급", "소비자_등심1등급")

# 숫자 변환
dd1 <- dd |>
  mutate(
    암송아지 = parse_number_simple(암송아지) * 1000,
    숫송아지 = parse_number_simple(숫송아지) * 1000,
    농가수취가격_600kg = parse_number_simple(농가수취가격_600kg) * 1000,
    지육_평균 = parse_number_simple(지육_평균),
    지육_1등급 = parse_number_simple(지육_1등급),
    도매_등심1등급 = parse_number_simple(도매_등심1등급),
    소비자_등심1등급 = parse_number_simple(소비자_등심1등급),
    명절 = NA
  ) |>
  select(date, 명절, everything())

# ============================================
# Google Sheets 읽기 & 쓰기
# ============================================
sheet_url <- "https://docs.google.com/spreadsheets/d/1baViBjPUz1wyicG-I-vN5RhyoCcFt-6lw550evTowEg/edit#gid=1181990123"

df <- read_sheet(sheet_url, sheet = 1)

# wday 컬럼 제거 후 재생성
if ("wday" %in% names(df)) {
  df <- df |> select(-wday)
}
df <- unique(df)
df$wday <- weekdays(as.Date(df$date), abbreviate = TRUE)

# 새 데이터에 날짜 관련 컬럼 추가
df1 <- dd1 |>
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    week = as.integer(format(date, "%V")),
    wday = weekdays(date, abbreviate = TRUE)
  )

# 기존 데이터에서 새 데이터에 없는 날짜만 필터
df$date <- as.Date(df$date)
df2 <- df |>
  filter(!date %in% df1$date)

# 합치기
df_final <- bind_rows(df1, df2)

message("총 ", nrow(df_final), " 행의 데이터")
print(head(df_final))

# Google Sheets에 쓰기
write_sheet(data = df_final, sheet_url, sheet = 1)

# 로컬 파일로 저장
dir.create("data_hanwoo_stock", showWarnings = FALSE)
filename <- paste0("data_hanwoo_stock/hanwoo-stock-", Sys.Date(), ".csv")
write.csv(df_final, file = filename, row.names = FALSE)

message("✅ 데이터 업데이트 완료: ", Sys.Date())
