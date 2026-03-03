# EKAPE Parsing Script for GitHub Actions
# Google Drive 없이 로컬 CSV로 데이터 관리

# Load libraries
library(dplyr)
library(rvest)
library(readr)

# ============================================
# 설정
# ============================================
DATA_DIR <- "data_hanwoo_stock"
CSV_FILE <- file.path(DATA_DIR, "hanwoo-stock.csv")

# ============================================
# 데이터 크롤링
# ============================================
dd <- rvest::read_html("https://www.ekapepia.com/supPrice/liveStock/type/cowNew.do?menuSn=86&boardInfoNo=&bbsSn=&userGroupType=40&paramSearchTxt=&pstSn=0") |>
  html_table()

dd <- dd[[1]]
dd <- dd[-c(1:2), ]

names(dd) <- c("date", "암송아지", "숫송아지", "농가수취가격_600kg", "지육_평균", "지육_1등급", "도매_등심1등급", "소비자_등심1등급")

# 숫자 변환 (readr::parse_number 사용 - 천단위 구분자 올바르게 처리)
dd1 <- dd |>
  mutate(
    across(암송아지:소비자_등심1등급, parse_number),
    across(암송아지:농가수취가격_600kg, ~ .x * 1000),
    명절 = NA
  ) |>
  select(date, 명절, everything())

# ============================================
# 기존 CSV 읽기 또는 새로 생성
# ============================================
dir.create(DATA_DIR, showWarnings = FALSE)

if (file.exists(CSV_FILE)) {
  message("📂 기존 CSV 파일 읽는 중: ", CSV_FILE)
  df <- read.csv(CSV_FILE, stringsAsFactors = FALSE)
  df$date <- as.Date(df$date)
} else {
  message("📄 새 CSV 파일 생성")
  df <- data.frame()
}

# 새 데이터에 날짜 관련 컬럼 추가
df1 <- dd1 |>
  mutate(
    date = as.Date(date),
    year = as.integer(format(date, "%Y")),
    week = as.integer(format(date, "%V")),
    wday = weekdays(date, abbreviate = TRUE)
  )

# 기존 데이터가 있으면 병합
if (nrow(df) > 0) {
  # wday 컬럼 제거 후 재생성
  if ("wday" %in% names(df)) {
    df <- df |> select(-wday)
  }
  df <- unique(df)
  df$wday <- weekdays(as.Date(df$date), abbreviate = TRUE)
  
  # 기존 데이터에서 새 데이터에 없는 날짜만 필터
  df2 <- df |>
    filter(!date %in% df1$date)
  
  # 합치기
  df_final <- bind_rows(df1, df2)
} else {
  df_final <- df1
}

# 날짜 정렬
df_final <- df_final |> arrange(desc(date))

message("총 ", nrow(df_final), " 행의 데이터")
print(head(df_final))

# ============================================
# CSV로 저장
# ============================================
write.csv(df_final, file = CSV_FILE, row.names = FALSE, fileEncoding = "UTF-8")

message("✅ 데이터 업데이트 완료: ", Sys.Date())
message("📁 저장 위치: ", CSV_FILE)
