# 기존 xlsx 파일에서 CSV 초기화 (일회성 실행)
# 가장 최신 xlsx 파일의 데이터를 hanwoo-stock.csv로 변환

library(dplyr)
library(readxl)

DATA_DIR <- "data_hanwoo_stock"
CSV_FILE <- file.path(DATA_DIR, "hanwoo-stock.csv")

# xlsx 파일 목록
xlsx_files <- list.files(DATA_DIR, pattern = "\\.xlsx$", full.names = TRUE)

if (length(xlsx_files) == 0) {
  stop("xlsx 파일이 없습니다.")
}

# 가장 최신 파일 선택 (파일명에 날짜가 있으므로 정렬로 최신 파일 선택)
latest_xlsx <- sort(xlsx_files, decreasing = TRUE)[1]
message("📂 가장 최신 xlsx 파일: ", latest_xlsx)

# xlsx 읽기
df <- read_excel(latest_xlsx)

# 날짜 형식 정리
df$date <- as.Date(df$date)

# year, week, wday 컬럼 확인 및 생성
if (!"year" %in% names(df)) {
  df$year <- as.integer(format(df$date, "%Y"))
}
if (!"week" %in% names(df)) {
  df$week <- as.integer(format(df$date, "%V"))
}
if (!"wday" %in% names(df)) {
  df$wday <- weekdays(df$date, abbreviate = TRUE)
}

# 중복 제거 및 정렬
df <- df |> 
  unique() |>
  arrange(desc(date))

message("총 ", nrow(df), " 행의 데이터")
print(head(df))

# CSV로 저장
write.csv(df, file = CSV_FILE, row.names = FALSE, fileEncoding = "UTF-8")

message("✅ CSV 초기화 완료: ", CSV_FILE)
