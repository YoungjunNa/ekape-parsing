# =============================================================================
# Prophet 하이퍼파라미터 그리드 서치 (로컬 1회 실행용)
# - 결과를 ekape-forecast.R 상단의 BEST_PARAMS 상수로 하드코딩하면 됨
# - CI에서는 실행하지 않음 (시간 보호)
# =============================================================================

suppressPackageStartupMessages({
  library(prophet)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(tidyr)
})

.data <- rlang::.data
WINSORIZE_QUANTILES <- c(0.01, 0.99)

source("ekape-forecast.R", local = TRUE, echo = FALSE)
# create_korean_holidays() 등 함수 재사용

# 그리드 정의 (changepoint × seasonality × holidays = 16조합)
# changepoint_prior_scale이 가장 영향력 큰 파라미터이므로 더 촘촘하게
grid <- expand_grid(
  changepoint_prior_scale = c(0.001, 0.01, 0.05, 0.5),
  seasonality_prior_scale = c(1.0, 10.0),
  holidays_prior_scale    = c(1.0, 10.0)
)

# 데이터 로드
data <- read_csv("data_hanwoo_stock/hanwoo-stock.csv", show_col_types = FALSE)
holidays <- create_korean_holidays()

# 평가용 외생 회귀변수 (송아지에만 사용)
jiyuk_hist <- data %>%
  transmute(ds = as.Date(date), 지육_평균 = .data$지육_평균) %>%
  filter(!is.na(.data$지육_평균))

# 그리드 1회 평가 함수
tune_one <- function(target_col, params, regressor_df = NULL) {
  df_prophet <- data %>%
    filter(date > as.Date("2018-01-01")) %>%
    filter(wday %in% c("Tue", "Wed", "Thu", "Fri")) %>%
    select(ds = date, y = !!sym(target_col)) %>%
    filter(!is.na(.data$y)) %>%
    arrange(.data$ds)

  regressor_cols <- character(0)
  if (!is.null(regressor_df)) {
    regressor_cols <- setdiff(names(regressor_df), "ds")
    df_prophet <- df_prophet %>%
      left_join(regressor_df, by = "ds") %>%
      filter(if_all(all_of(regressor_cols), ~ !is.na(.)))
  }

  if (nrow(df_prophet) >= 50) {
    qs <- quantile(df_prophet$y, probs = WINSORIZE_QUANTILES, na.rm = TRUE)
    df_prophet <- df_prophet %>%
      mutate(y = pmin(pmax(.data$y, qs[1]), qs[2]))
  }

  m <- prophet(
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
    m <- add_regressor(m, rcol, prior.scale = 0.5,
                       mode = "additive", standardize = TRUE)
  }
  m <- fit.prophet(m, df_prophet)

  # 튜닝용 더 빠른 CV (initial 5년 → 폴드 수 감소)
  cv <- cross_validation(m, initial = 1825, period = 180, horizon = 180,
                         units = "days")
  pm <- performance_metrics(cv, metrics = "mape")
  mean(pm$mape, na.rm = TRUE)
}

run_grid <- function(target_col, regressor_df = NULL) {
  cat("\n=== Tuning:", target_col, "===\n")
  results <- grid %>%
    rowwise() %>%
    mutate(
      MAPE = {
        cat(sprintf("  cps=%.3f sps=%.1f hps=%.1f ... ",
                    .data$changepoint_prior_scale, .data$seasonality_prior_scale,
                    .data$holidays_prior_scale))
        m <- tryCatch(
          tune_one(target_col, list(
            changepoint_prior_scale = .data$changepoint_prior_scale,
            seasonality_prior_scale = .data$seasonality_prior_scale,
            holidays_prior_scale    = .data$holidays_prior_scale
          ), regressor_df),
          error = function(e) NA_real_
        )
        cat(sprintf("MAPE=%.4f\n", m))
        m
      }
    ) %>%
    ungroup() %>%
    arrange(.data$MAPE)
  print(results, n = Inf)
  cat("\n>>> BEST for", target_col, ":\n")
  print(head(results, 1))
  results
}

cat("========================================\n")
cat("Prophet 하이퍼파라미터 그리드 서치 시작\n")
cat("그리드 크기:", nrow(grid), "조합 × 3변수\n")
cat("========================================\n")

res_jiyuk  <- run_grid("지육_평균")
res_amsong <- run_grid("암송아지", regressor_df = jiyuk_hist)
res_susong <- run_grid("숫송아지", regressor_df = jiyuk_hist)

cat("\n========================================\n")
cat("최종 권장 파라미터\n")
cat("========================================\n")
cat("지육_평균 :"); print(head(res_jiyuk[, 1:3], 1))
cat("암송아지  :"); print(head(res_amsong[, 1:3], 1))
cat("숫송아지  :"); print(head(res_susong[, 1:3], 1))
