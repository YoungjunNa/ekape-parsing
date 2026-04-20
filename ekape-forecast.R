# =============================================================================
# 한우 가격 예측 스크립트 (Prophet 기반)
# - 예측 대상: 암송아지, 숫송아지, 지육_평균
# - 예측 기간: 365일 (1년)
# - 주말/공휴일 제외, 화요일 가격 하락 효과 반영
# - 저장 위치: data_hanwoo_stock/hanwoo-forecast.csv
# =============================================================================

library(prophet)
library(dplyr)
library(readr)
library(lubridate)

# =============================================================================
# 1. 한국 공휴일 데이터프레임 생성
# =============================================================================

create_korean_holidays <- function() {

  # 고정 공휴일 (2026-2027년)
  fixed_holidays <- tribble(
    ~holiday,         ~ds,
    # 2026년 고정 공휴일
    "신정",           "2026-01-01",
    "삼일절",         "2026-03-01",
    "어린이날",       "2026-05-05",
    "현충일",         "2026-06-06",
    "광복절",         "2026-08-15",
    "개천절",         "2026-10-03",
    "한글날",         "2026-10-09",
    "성탄절",         "2026-12-25",
    # 2027년 고정 공휴일
    "신정",           "2027-01-01",
    "삼일절",         "2027-03-01",
    "어린이날",       "2027-05-05",
    "현충일",         "2027-06-06",
    "광복절",         "2027-08-15",
    "개천절",         "2027-10-03",
    "한글날",         "2027-10-09",
    "성탄절",         "2027-12-25"
  )
  

  # 음력 기반 공휴일 (연도별로 다름)
  lunar_holidays <- tribble(
    ~holiday,           ~ds,
    # 2026년 설날 연휴 (음력 1/1 = 양력 2/17)
    "설날연휴",         "2026-02-16",
    "설날",             "2026-02-17",
    "설날연휴",         "2026-02-18",
    # 2026년 부처님오신날 (음력 4/8 = 양력 5/24)
    "부처님오신날",     "2026-05-24",
    # 2026년 추석 연휴 (음력 8/15 = 양력 10/5)
    "추석연휴",         "2026-10-04",
    "추석",             "2026-10-05",
    "추석연휴",         "2026-10-06",
    
    # 2027년 설날 연휴 (음력 1/1 = 양력 2/6)
    "설날연휴",         "2027-02-05",
    "설날",             "2027-02-06",
    "설날연휴",         "2027-02-07",
    # 2027년 부처님오신날 (음력 4/8 = 양력 5/13)
    "부처님오신날",     "2027-05-13",
    # 2027년 추석 연휴 (음력 8/15 = 양력 9/25)
    "추석연휴",         "2027-09-24",
    "추석",             "2027-09-25",
    "추석연휴",         "2027-09-26"
  )
  
  # 대체공휴일 (일요일/공휴일 겹칠 시)
  substitute_holidays <- tribble(
    ~holiday,           ~ds,
    # 2026년 대체공휴일
    "삼일절대체",       "2026-03-02",   # 3/1 일요일
    # 2027년 대체공휴일
    "어린이날대체",     "2027-05-06",   # 5/5 수요일이지만 석가탄신일과 가까워 연휴
    "한글날대체",       "2027-10-11"    # 10/9 토요일
  )
  
  # 모든 공휴일 합치기
  holidays <- bind_rows(fixed_holidays, lunar_holidays, substitute_holidays) %>%
    mutate(ds = as.Date(ds))
  
  return(holidays)
}

# =============================================================================
# 2. Prophet 모델 훈련 및 예측 함수
# =============================================================================

forecast_variable <- function(data, target_col, holidays, periods = 365, forecast_start_date = NULL) {
  # Prophet 형식으로 데이터 변환 (ds, y)
  # 화수목금 데이터만 필터링 (주말/월요일 제외)
  # wday: 1=일, 2=월, 3=화, 4=수, 5=목, 6=금, 7=토
  df_prophet <- data %>%
    filter(date > as.Date("2018-01-01")) %>%
    filter(wday %in% c("Tue", "Wed", "Thu", "Fri")) %>%
    select(ds = date, y = !!sym(target_col)) %>%
    filter(!is.na(y)) %>%
    arrange(ds) %>%
    # 화요일 효과 추가 (쉬는 날 다음날 = 화요일, 가격 하락 패턴)
    mutate(is_after_break = as.integer(wday(ds, label = FALSE) == 3))  # 3 = Tuesday
  
  # 데이터가 충분한지 확인
  if (nrow(df_prophet) < 14) {
    warning(paste0("데이터가 부족합니다: ", target_col, " (", nrow(df_prophet), "행)"))
    return(NULL)
  }
  
  # Prophet 모델 생성 (yearly, weekly seasonality 활성화)
  model <- prophet(
    holidays = holidays,
    yearly.seasonality = TRUE,    # 연간 계절성 활성화
    weekly.seasonality = TRUE,    # 주간 패턴 반영
    daily.seasonality = FALSE,    # 일간 패턴 비활성화 (평일만 사용)
    interval.width = 0.80         # 80% 신뢰구간
  )
  
  # 화요일 효과 regressor 추가 (쉬는 날 다음날 가격 하락)
  model <- add_regressor(model, "is_after_break", prior.scale = 10, mode = "additive")
  
  # 모델 훈련
  model <- fit.prophet(model, df_prophet)
  
  # 미래 날짜 생성 (충분히 생성 후 평일만 필터링)
  future_all <- make_future_dataframe(model, periods = periods * 2)  # 주말 제외 위해 여유있게 생성
  
  # 평일만 필터링 (화-금: 3,4,5,6) + 화요일 효과 추가
  future <- future_all %>%
    mutate(weekday_num = wday(ds, label = FALSE)) %>%
    filter(weekday_num %in% c(3, 4, 5, 6)) %>%  # 화수목금만 (3=Tue, 4=Wed, 5=Thu, 6=Fri)
    mutate(is_after_break = as.integer(weekday_num == 3)) %>%  # 3 = Tuesday
    select(ds, is_after_break)
  
  # 예측 수행
  forecast <- predict(model, future)
  
  # 미래 예측값만 추출 (공통 시작일 사용)
  if (is.null(forecast_start_date)) {
    forecast_start_date <- max(df_prophet$ds)
  }
  
  # periods 개수만큼만 추출
  forecast_future <- forecast %>%
    filter(as.Date(ds) > as.Date(forecast_start_date)) %>%
    mutate(ds = as.Date(ds)) %>%
    select(ds, yhat, yhat_lower, yhat_upper) %>%
    head(periods)  # 정확히 periods 개수만
  
  # 컬럼명 변경
  names(forecast_future) <- c("date", 
                               paste0(target_col, "_예측"),
                               paste0(target_col, "_하한"),
                               paste0(target_col, "_상한"))
  
  return(forecast_future)
}

# =============================================================================
# 3. 메인 실행
# =============================================================================

main <- function() {
  cat("========================================\n")
  cat("한우 가격 예측 시작\n")
  cat("예측 대상: 암송아지, 숫송아지, 지육_평균\n")
  cat("예측 기간: 365일 (1년, 평일만)\n")
  cat("========================================\n\n")
  
  # 데이터 읽기
  data_path <- "data_hanwoo_stock/hanwoo-stock.csv"
  
  if (!file.exists(data_path)) {
    stop("데이터 파일을 찾을 수 없습니다: ", data_path)
  }
  
  data <- read_csv(data_path, show_col_types = FALSE)
  cat("데이터 로드 완료:", nrow(data), "행\n\n")
  
  # 공휴일 데이터 생성
  holidays <- create_korean_holidays()
  cat("공휴일 데이터 생성 완료:", nrow(holidays), "개 공휴일\n\n")
  
  # 예측 대상 변수
  target_vars <- c("암송아지", "숫송아지", "지육_평균")
  
  # 공통 예측 시작일 계산 (모든 변수 중 가장 최근 날짜 기준)
  forecast_start_date <- data %>%
    select(date, all_of(target_vars)) %>%
    tidyr::pivot_longer(cols = all_of(target_vars), names_to = "var", values_to = "value") %>%
    filter(!is.na(value)) %>%
    summarise(max_date = max(date)) %>%
    pull(max_date)
  
  cat("예측 시작일:", as.character(forecast_start_date + 1), "\n\n")
  
  # 각 변수별 예측 수행
  forecasts <- list()
  
  for (var in target_vars) {
    cat("예측 중:", var, "...\n")
    
    forecast_result <- forecast_variable(data, var, holidays, periods = 260,  # 약 1년치 평일 (52주 * 5일 = 260일, 화-금만 약 208일)
                                          forecast_start_date = forecast_start_date)
    
    if (!is.null(forecast_result)) {
      forecasts[[var]] <- forecast_result
      cat("  완료! (", nrow(forecast_result), "일 예측)\n")
    } else {
      cat("  실패 - 데이터 부족\n")
    }
  }
  
  # 예측 결과 통합
  if (length(forecasts) == 0) {
    stop("예측 결과가 없습니다.")
  }
  
  cat("\n예측 결과 통합 중...\n")
  
  # 첫 번째 예측 결과를 기준으로 병합
  combined_forecast <- forecasts[[1]]
  
  if (length(forecasts) > 1) {
    for (i in 2:length(forecasts)) {
      combined_forecast <- combined_forecast %>%
        left_join(forecasts[[i]], by = "date")
    }
  }
  
  # 날짜순 정렬 및 날짜 형식 변환
  combined_forecast <- combined_forecast %>%
    arrange(date) %>%
    mutate(date = format(date, "%Y-%m-%d"))
  
  # 숫자 반올림 (가격은 정수)
  numeric_cols <- names(combined_forecast)[names(combined_forecast) != "date"]
  combined_forecast <- combined_forecast %>%
    mutate(across(all_of(numeric_cols), ~ round(.x, 0)))
  
  # CSV 저장
  output_path <- "data_hanwoo_stock/hanwoo-forecast.csv"
  write_csv(combined_forecast, output_path)
  
  cat("\n========================================\n")
  cat("예측 완료!\n")
  cat("저장 위치:", output_path, "\n")
  cat("예측 기간:", min(combined_forecast$date), "~", max(combined_forecast$date), "\n")
  cat("총 예측일:", nrow(combined_forecast), "일\n")
  cat("========================================\n")
  
  # 예측 결과 미리보기
  cat("\n[예측 결과 미리보기 - 처음 5일]\n")
  print(head(combined_forecast, 5))
  
  return(combined_forecast)
}

# 스크립트 실행
if (!interactive()) {
  main()
} else {
  # 대화형 환경에서는 main() 직접 호출
  # result <- main()
}
