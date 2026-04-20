# =============================================================================
# 한우 가격 예측 시각화 스크립트
# - 최근 3개월 실제 가격 + 1년 예측값 그래프 생성
# - 주차별 평균 가격으로 표시
# - 저장 위치: assets/forecast-plot.png
# =============================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

# 디렉토리 생성
if (!dir.exists("assets")) {
  dir.create("assets")
}

# =============================================================================
# 1. 데이터 로드
# =============================================================================

# 실제 가격 데이터
actual_data <- read_csv("data_hanwoo_stock/hanwoo-stock.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# 예측 데이터
forecast_data <- read_csv("data_hanwoo_stock/hanwoo-forecast.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# =============================================================================
# 2. 주차별 평균 계산 함수
# =============================================================================

calculate_weekly_avg <- function(data, value_col, type_label) {
  data %>%
    mutate(
      year_week = floor_date(date, "week", week_start = 1),  # 월요일 시작
      year = year(date),
      week = isoweek(date)
    ) %>%
    group_by(year_week) %>%
    summarise(
      가격 = mean(!!sym(value_col), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(가격)) %>%
    mutate(유형 = type_label)
}

# =============================================================================
# 3. 데이터 전처리 - 주차별 평균
# =============================================================================

# 최근 3개월 실제 데이터 (주차별 평균)
three_months_ago <- max(actual_data$date) - 90

actual_weekly <- list()
for (var in c("암송아지", "숫송아지", "지육_평균")) {
  actual_weekly[[var]] <- actual_data %>%
    filter(date >= three_months_ago) %>%
    calculate_weekly_avg(var, "실제") %>%
    mutate(변수 = var)
}
actual_weekly_all <- bind_rows(actual_weekly)

# 예측 데이터 (주차별 평균)
forecast_weekly <- list()
forecast_ci_weekly <- list()

for (var in c("암송아지", "숫송아지", "지육_평균")) {
  pred_col <- paste0(var, "_예측")
  lower_col <- paste0(var, "_하한")
  upper_col <- paste0(var, "_상한")
  
  # 예측값 주차별 평균
  forecast_weekly[[var]] <- forecast_data %>%
    calculate_weekly_avg(pred_col, "예측") %>%
    mutate(변수 = var)
  
  # 신뢰구간 주차별 평균
  forecast_ci_weekly[[var]] <- forecast_data %>%
    mutate(year_week = floor_date(date, "week", week_start = 1)) %>%
    group_by(year_week) %>%
    summarise(
      하한 = mean(!!sym(lower_col), na.rm = TRUE),
      상한 = mean(!!sym(upper_col), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(변수 = var)
}

forecast_weekly_all <- bind_rows(forecast_weekly)
forecast_ci_all <- bind_rows(forecast_ci_weekly)

# =============================================================================
# 4. 개별 변수 그래프 생성 함수 (주차별)
# =============================================================================

create_weekly_forecast_plot <- function(var_name, y_label, title_suffix) {
  
  # 실제 데이터 필터
  actual_var <- actual_weekly_all %>% filter(변수 == var_name)
  
  # 예측 데이터 필터
  forecast_var <- forecast_weekly_all %>% filter(변수 == var_name)
  
  # 신뢰구간 필터
  ci_var <- forecast_ci_all %>% filter(변수 == var_name)
  
  # 연결점 (마지막 실제 + 첫 예측)
  last_actual <- actual_var %>% filter(year_week == max(year_week))
  first_forecast <- forecast_var %>% filter(year_week == min(year_week))
  
  # 그래프 생성
  p <- ggplot() +
    # 신뢰구간 (80%)
    geom_ribbon(data = ci_var, aes(x = year_week, ymin = 하한, ymax = 상한),
                fill = "#3498db", alpha = 0.2) +
    # 실제 가격 선
    geom_line(data = actual_var, aes(x = year_week, y = 가격), 
              color = "#2c3e50", linewidth = 1.2) +
    # 실제 가격 점
    geom_point(data = actual_var, aes(x = year_week, y = 가격), 
               color = "#2c3e50", size = 2.5) +
    # 연결선 (실제 → 예측)
    geom_segment(aes(x = last_actual$year_week, xend = first_forecast$year_week,
                     y = last_actual$가격, yend = first_forecast$가격),
                 color = "#3498db", linetype = "dashed", linewidth = 1) +
    # 예측 가격 선
    geom_line(data = forecast_var, aes(x = year_week, y = 가격), 
              color = "#3498db", linewidth = 1.2) +
    # 예측 가격 점
    geom_point(data = forecast_var, aes(x = year_week, y = 가격), 
               color = "#3498db", size = 1.5, alpha = 0.7) +
    # 예측 시작 수직선
    geom_vline(xintercept = as.numeric(min(forecast_var$year_week)), 
               linetype = "dotted", color = "#e74c3c", linewidth = 0.8) +
    # 테마
    labs(
      title = paste0(title_suffix, " 주차별 평균 가격 예측"),
      subtitle = paste0("실제: ", format(min(actual_var$year_week), "%Y-%m-%d"), " ~ ", 
                        format(max(actual_var$year_week), "%Y-%m-%d"),
                        " | 예측: ", format(min(forecast_var$year_week), "%Y-%m-%d"), " ~ ",
                        format(max(forecast_var$year_week), "%Y-%m-%d")),
      x = "주차",
      y = y_label,
      caption = "파란 영역: 80% 신뢰구간 | 빨간 점선: 예측 시작"
    ) +
    theme_minimal(base_family = "AppleGothic") +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10),
      plot.caption = element_text(color = "gray50", size = 8),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "2 months") +
    scale_y_continuous(labels = scales::comma)
  
  return(p)
}

# =============================================================================
# 5. 3개 변수 그래프 생성 및 결합
# =============================================================================

# 개별 그래프 생성
p1 <- create_weekly_forecast_plot("암송아지", "가격 (원)", "암송아지")
p2 <- create_weekly_forecast_plot("숫송아지", "가격 (원)", "숫송아지")
p3 <- create_weekly_forecast_plot("지육_평균", "가격 (원/kg)", "지육 평균")

# patchwork로 결합
library(patchwork)

combined_plot <- p1 / p2 / p3 +
  plot_annotation(
    title = "한우 가격 예측 (Prophet 모델) - 주차별 평균",
    subtitle = paste0("생성일: ", Sys.Date(), " | 실제 3개월 + 예측 1년 (평일만, 화요일 효과 반영)"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, family = "AppleGothic"),
      plot.subtitle = element_text(color = "gray50", size = 11, family = "AppleGothic")
    )
  )

# =============================================================================
# 6. 저장
# =============================================================================

ggsave("assets/forecast-plot.png", combined_plot, 
       width = 12, height = 14, dpi = 150, bg = "white")

cat("그래프 저장 완료: assets/forecast-plot.png\n")
