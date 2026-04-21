# =============================================================================
# 한우 가격 예측 스크립트 (Prophet 기반)
# - 예측 대상: 암송아지, 숫송아지, 지육_평균
# - 예측 기간: 365일 (1년)
# - 주말/월요일 제외 (월요일은 일요일 휴장 영향으로 물량이 적어 가격 왜곡)
# - 화~금 데이터만 사용하여 안정적인 가격 패턴 학습
# - 저장 위치: data_hanwoo_stock/hanwoo-forecast.csv
# =============================================================================

library(prophet)
library(dplyr)
library(readr)
library(lubridate)
suppressPackageStartupMessages(library(forecast))

.data <- rlang::.data

# =============================================================================
# 전역 설정
# =============================================================================
# Prophet + ARIMA 앙상블 사용 여부 (가중치: prophet 0.7, arima 0.3)
ENABLE_ENSEMBLE <- TRUE
# 이상치 윈저라이즈 범위 (1분위수 ↔ 99분위수까지만 사용)
WINSORIZE_QUANTILES <- c(0.01, 0.99)

# =============================================================================
# 변수별 최적 하이퍼파라미터 (tune-prophet.R 그리드 서치 결과, 6개월 horizon MAPE 기준)
# - changepoint_prior_scale이 지배적 파라미터; 다른 프라이어는 영향 이해
# =============================================================================
BEST_PARAMS <- list(
  "지육_평균" = list(changepoint_prior_scale = 0.5,   seasonality_prior_scale = 10, holidays_prior_scale = 1),
  "암송아지"  = list(changepoint_prior_scale = 0.05,  seasonality_prior_scale = 1,  holidays_prior_scale = 1),
  "숫송아지"  = list(changepoint_prior_scale = 0.001, seasonality_prior_scale = 10, holidays_prior_scale = 1)
)

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
  
  # 모든 공휴일 합치기 + 명절 전후 수요 급증 윈도우 부여
  # - 설날/추석: 연휴 1주 전부터 수요 급증, 종료 3일 후까지 쟔재
  # - 명절급 단일 공휴일(어린이날·부처님오신날): 전 3일 수요 증가
  # - 그 외 단일 공휴일: 전후 1일
  holidays <- bind_rows(fixed_holidays, lunar_holidays, substitute_holidays) %>%
    mutate(
      ds = as.Date(.data$ds),
      lower_window = case_when(
        holiday %in% c("설날", "설날연휴", "추석", "추석연휴")    ~ -7L,
        holiday %in% c("부처님오신날", "어린이날", "어린이날대체") ~ -3L,
        TRUE                                                          ~ -1L
      ),
      upper_window = case_when(
        holiday %in% c("설날", "설날연휴", "추석", "추석연휴")    ~ 3L,
        holiday %in% c("부처님오신날", "어린이날", "어린이날대체") ~ 1L,
        TRUE                                                          ~ 1L
      )
    )

  return(holidays)
}

# =============================================================================
# 2. Prophet 모델 훈련 및 예측 함수
# =============================================================================

forecast_variable <- function(data, target_col, holidays, periods = 365,
                              forecast_start_date = NULL,
                              regressor_df = NULL,
                              params = NULL) {
  # 변수별 최적 파라미터 (지정되지 않으면 BEST_PARAMS에서 조회)
  if (is.null(params)) {
    params <- BEST_PARAMS[[target_col]]
    if (is.null(params)) {
      params <- list(changepoint_prior_scale = 0.05,
                     seasonality_prior_scale = 10,
                     holidays_prior_scale = 10)
    }
  }

  # Prophet 형식으로 데이터 변환 (ds, y)
  # 화수목금 데이터만 필터링 (주말/월요일 제외)
  # wday: 1=일, 2=월, 3=화, 4=수, 5=목, 6=금, 7=토
  df_prophet <- data %>%
    filter(date > as.Date("2018-01-01")) %>%
    filter(wday %in% c("Tue", "Wed", "Thu", "Fri")) %>%
    select(ds = date, y = !!sym(target_col)) %>%
    filter(!is.na(.data$y)) %>%
    arrange(.data$ds)

  if (nrow(df_prophet) >= 50) {
    qs <- quantile(df_prophet$y, probs = WINSORIZE_QUANTILES, na.rm = TRUE)
    df_prophet <- df_prophet %>%
      mutate(y = pmin(pmax(.data$y, qs[1]), qs[2]))
  }

  # 외생 회귀변수 조인 (학습 데이터에 NA 있는 행 제거)
  regressor_cols <- character(0)
  if (!is.null(regressor_df)) {
    regressor_cols <- setdiff(names(regressor_df), "ds")
    df_prophet <- df_prophet %>%
      left_join(regressor_df, by = "ds") %>%
      filter(if_all(all_of(regressor_cols), ~ !is.na(.)))
  }

  # 데이터가 충분한지 확인
  if (nrow(df_prophet) < 14) {
    warning(paste0("데이터가 부족합니다: ", target_col, " (", nrow(df_prophet), "행)"))
    return(NULL)
  }

  # Prophet 모델 생성
  # 화~금 데이터만 사용하므로 요일별 가격 차이는 거의 없음 → weekly seasonality 비활성화
  # 가격은 양수이고 변동폭이 수준에 비례 → multiplicative seasonality 사용
  model <- prophet(
    holidays = holidays,
    yearly.seasonality = TRUE,    # 연간 계절성 활성화
    weekly.seasonality = FALSE,   # 화~금 데이터만 사용 (요일 효과 무시)
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",  # 수준에 비례하는 변동 반영
    interval.width = 0.80,        # 80% 신뢰구간
    changepoint.prior.scale = params$changepoint_prior_scale,
    seasonality.prior.scale = params$seasonality_prior_scale,
    holidays.prior.scale    = params$holidays_prior_scale
  )

  # 외생 회귀변수 등록
  for (rcol in regressor_cols) {
    model <- add_regressor(model, rcol, prior.scale = 0.5,
                           mode = "additive", standardize = TRUE)
  }

  # 모델 훈련
  model <- fit.prophet(model, df_prophet)

  # 미래 날짜 생성 (충분히 생성 후 평일만 필터링)
  future_all <- make_future_dataframe(model, periods = periods * 2)  # 주말 제외 위해 여유있게 생성

  # 평일만 필터링 (화-금: 3,4,5,6)
  future <- future_all %>%
    mutate(weekday_num = wday(.data$ds, label = FALSE)) %>%
    filter(.data$weekday_num %in% c(3, 4, 5, 6)) %>%  # 화수목금만 (3=Tue, 4=Wed, 5=Thu, 6=Fri)
    select(ds = .data$ds)

  # 외생 회귀변수를 future에 조인 (NA 있는 날짜 제거)
  if (length(regressor_cols) > 0) {
    future <- future %>%
      mutate(ds_date = as.Date(.data$ds)) %>%
      left_join(
        regressor_df %>% rename(ds_date = .data$ds),
        by = "ds_date"
      ) %>%
      select(-all_of("ds_date")) %>%
      filter(if_all(all_of(regressor_cols), ~ !is.na(.)))
  }

  # 예측 수행 (Prophet)
  forecast <- predict(model, future)

  # ARIMA 앙상블 (외생 회귀변수 없을 때만 적용 — 아래 설명)
  # ARIMA는 회귀변수를 포함시키려면 xreg 용 미래값이 필요해 복잡도 ↑
  # 송아지는 이미 지육_평균 회귀변수 이득이 크므로 Prophet 단독 유지
  if (ENABLE_ENSEMBLE && length(regressor_cols) == 0) {
    arima_yhat <- tryCatch({
      ts_y <- df_prophet$y
      arima_fit <- forecast::auto.arima(ts_y, seasonal = FALSE,
                                        stepwise = TRUE, approximation = TRUE)
      h <- nrow(future) - nrow(df_prophet)
      if (h > 0) {
        arima_fc <- forecast::forecast(arima_fit, h = h)
        c(fitted(arima_fit), as.numeric(arima_fc$mean))
      } else {
        as.numeric(fitted(arima_fit))
      }
    }, error = function(e) { message("ARIMA 실패: ", conditionMessage(e)); NULL })

    if (!is.null(arima_yhat) && length(arima_yhat) == nrow(forecast)) {
      # 앙상블: Prophet 0.7 + ARIMA 0.3 (Prophet이 계절성/공휴일 처리가 더 신뢰성 높음)
      forecast$yhat       <- 0.7 * forecast$yhat       + 0.3 * arima_yhat
      forecast$yhat_lower <- 0.7 * forecast$yhat_lower + 0.3 * arima_yhat
      forecast$yhat_upper <- 0.7 * forecast$yhat_upper + 0.3 * arima_yhat
    }
  }
  
  # 미래 예측값만 추출 (공통 시작일 사용)
  if (is.null(forecast_start_date)) {
    forecast_start_date <- max(df_prophet$ds)
  }
  
  # periods 개수만큼만 추출
  forecast_future <- forecast %>%
    filter(as.Date(.data$ds) > as.Date(forecast_start_date)) %>%
    mutate(ds = as.Date(.data$ds)) %>%
    transmute(ds = .data$ds, yhat = .data$yhat, yhat_lower = .data$yhat_lower, yhat_upper = .data$yhat_upper) %>%
    head(periods)  # 정확히 periods 개수만
  
  # 컬럼명 변경
  names(forecast_future) <- c("date", 
                               paste0(target_col, "_예측"),
                               paste0(target_col, "_하한"),
                               paste0(target_col, "_상한"))
  
  return(forecast_future)
}

# =============================================================================
# 2-1. 모델 정확도 평가 (Prophet cross_validation 기반, MAPE/RMSE/MAE)
# =============================================================================

evaluate_model <- function(data, target_col, holidays,
                          initial = 1095,
                          period  = 180,
                          horizon = 180,
                          regressor_df = NULL,
                          params = NULL) {
  if (is.null(params)) {
    params <- BEST_PARAMS[[target_col]]
    if (is.null(params)) {
      params <- list(changepoint_prior_scale = 0.05,
                     seasonality_prior_scale = 10,
                     holidays_prior_scale = 10)
    }
  }

  df_prophet <- data %>%
    filter(date > as.Date("2018-01-01")) %>%
    filter(wday %in% c("Tue", "Wed", "Thu", "Fri")) %>%
    select(ds = date, y = !!sym(target_col)) %>%
    filter(!is.na(.data$y)) %>%
    arrange(.data$ds)

  # 이상치 완화 (winsorize) — forecast_variable()와 동일 처리로 평가 일관성 유지
  if (nrow(df_prophet) >= 50) {
    qs <- quantile(df_prophet$y, probs = WINSORIZE_QUANTILES, na.rm = TRUE)
    df_prophet <- df_prophet %>%
      mutate(y = pmin(pmax(.data$y, qs[1]), qs[2]))
  }

  regressor_cols <- character(0)
  if (!is.null(regressor_df)) {
    regressor_cols <- setdiff(names(regressor_df), "ds")
    df_prophet <- df_prophet %>%
      left_join(regressor_df, by = "ds") %>%
      filter(if_all(all_of(regressor_cols), ~ !is.na(.)))
  }

  if (nrow(df_prophet) < 800) {
    warning(paste0("교차검증을 위한 데이터가 부족합니다: ", target_col,
                   " (", nrow(df_prophet), "행)"))
    return(NULL)
  }

  model <- prophet(
    holidays = holidays,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",
    interval.width = 0.80,
    changepoint.prior.scale = params$changepoint_prior_scale,
    seasonality.prior.scale = params$seasonality_prior_scale,
    holidays.prior.scale    = params$holidays_prior_scale
  )
  for (rcol in regressor_cols) {
    model <- add_regressor(model, rcol, prior.scale = 0.5,
                           mode = "additive", standardize = TRUE)
  }
  model <- fit.prophet(model, df_prophet)

  cv <- cross_validation(model,
                         initial = initial,
                         period  = period,
                         horizon = horizon,
                         units   = "days")
  pm <- performance_metrics(cv, metrics = c("mape", "rmse", "mae"))

  tibble::tibble(
    변수 = target_col,
    MAPE = mean(pm$mape, na.rm = TRUE),
    RMSE = mean(pm$rmse, na.rm = TRUE),
    MAE  = mean(pm$mae,  na.rm = TRUE),
    n_cv = nrow(cv)
  )
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
    filter(!is.na(.data$value)) %>%
    summarise(max_date = max(date)) %>%
    pull(.data$max_date)
  
  cat("예측 시작일:", as.character(forecast_start_date + 1), "\n\n")
  
  # 각 변수별 예측 수행
  # 1) 지육_평균 단변량 예측 (송아지 모델의 외생 회귀변수로 사용)
  # 2) 송아지 예측 시 지육_평균을 회귀변수로 추가
  forecasts <- list()

  # 1) 지육_평균 먼저 예측
  cat("예측 중: 지육_평균 (단변량) ...\n")
  forecasts[["지육_평균"]] <- forecast_variable(
    data, "지육_평균", holidays, periods = 260,
    forecast_start_date = forecast_start_date
  )
  if (!is.null(forecasts[["지육_평균"]])) {
    cat("  완료! (", nrow(forecasts[["지육_평균"]]), "일 예측)\n")
  }

  # 2) 송아지용 외생 회귀변수 구성: 과거 실제 지육_평균 + 미래 예측값
  jiyuk_regressor <- bind_rows(
    data %>%
      transmute(ds = as.Date(date), 지육_평균 = .data$지육_평균) %>%
      filter(!is.na(.data$지육_평균)),
    forecasts[["지육_평균"]] %>%
      transmute(ds = as.Date(date), 지육_평균 = .data$지육_평균_예측)
  ) %>%
    distinct(.data$ds, .keep_all = TRUE) %>%
    arrange(.data$ds)

  # 3) 송아지 예측 (지육_평균을 회귀변수로 사용)
  for (var in c("암송아지", "숫송아지")) {
    cat("예측 중:", var, "(지육_평균 회귀변수 포함) ...\n")
    forecast_result <- forecast_variable(
      data, var, holidays, periods = 260,
      forecast_start_date = forecast_start_date,
      regressor_df = jiyuk_regressor
    )
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

  # 모델 정확도 평가 (6개월 horizon 교차검증)
  # SKIP_EVAL=1 환경변수로 건너뛸 수 있음 (CI 시간 절약)
  if (Sys.getenv("SKIP_EVAL") != "1") {
    cat("\n========================================\n")
    cat("모델 정확도 평가 (6개월 horizon, 교차검증)\n")
    cat("========================================\n")

    eval_results <- list()
    # 평가용 외생 회귀변수 (과거 실제 지육_평균)
    jiyuk_hist <- data %>%
      transmute(ds = as.Date(date), 지육_평균 = .data$지육_평균) %>%
      filter(!is.na(.data$지육_평균))

    for (var in target_vars) {
      cat("평가 중:", var, "...\n")
      reg <- if (var %in% c("암송아지", "숫송아지")) jiyuk_hist else NULL
      res <- tryCatch(
        evaluate_model(data, var, holidays, regressor_df = reg),
        error = function(e) { cat("  실패:", conditionMessage(e), "\n"); NULL }
      )
      if (!is.null(res)) eval_results[[var]] <- res
    }

    if (length(eval_results) > 0) {
      metrics_df <- bind_rows(eval_results) %>%
        mutate(
          MAPE = sprintf("%.2f%%", .data$MAPE * 100),
          RMSE = formatC(.data$RMSE, format = "d", big.mark = ","),
          MAE  = formatC(.data$MAE,  format = "d", big.mark = ",")
        )
      cat("\n")
      print(metrics_df)
    }
  }

  return(combined_forecast)
}

# 스크립트 실행 (직접 실행 시에만 main() 호출, source() 시에는 함수만 로드)
if (!interactive() && sys.nframe() == 0L) {
  main()
}
