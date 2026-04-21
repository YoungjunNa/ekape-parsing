# =============================================================================
# 한우 가격 예측 시각화 스크립트
# - 최근 6개월 실제 가격 + 향후 6개월 예측값 그래프 생성
# - 주차별 평균 가격으로 표시
# - 저장 위치: assets/forecast-plot.png
# =============================================================================

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(prophet)
library(patchwork)

.data <- rlang::.data

# =============================================================================
# 한글 폰트 설정 (OS별 자동 선택)
# - macOS: AppleGothic
# - Linux(GitHub Actions): Noto Sans CJK KR (fonts-noto-cjk 패키지)
# - Windows: 맑은 고딕
# 시스템에 설치된 폰트 중 사용 가능한 것을 자동으로 선택한다.
# =============================================================================

resolve_korean_font <- function() {
  sysname <- Sys.info()[["sysname"]]

  candidates <- switch(
    sysname,
    "Darwin"  = c("AppleGothic", "Apple SD Gothic Neo", "Noto Sans CJK KR", "NanumGothic"),
    "Linux"   = c("Noto Sans CJK KR", "NanumGothic", "NanumBarunGothic", "UnDotum"),
    "Windows" = c("Malgun Gothic", "맑은 고딕", "NanumGothic", "Noto Sans CJK KR"),
    c("Noto Sans CJK KR", "NanumGothic", "AppleGothic")
  )

  available <- tryCatch(
    as.character(systemfonts::system_fonts()$family),
    error = function(e) character(0)
  )

  if (length(available) > 0) {
    hit <- candidates[candidates %in% available]
    if (length(hit) > 0) return(hit[[1]])
  }

  candidates[[1]]
}

korean_font <- resolve_korean_font()
message("Using Korean font family: ", korean_font)

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
# 1-1. 한국 공휴일 데이터프레임 생성
# =============================================================================

create_korean_holidays <- function() {
  fixed_holidays <- tibble::tribble(
    ~holiday,     ~ds,
    "신정",       "2026-01-01",
    "삼일절",     "2026-03-01",
    "어린이날",   "2026-05-05",
    "현충일",     "2026-06-06",
    "광복절",     "2026-08-15",
    "개천절",     "2026-10-03",
    "한글날",     "2026-10-09",
    "성탄절",     "2026-12-25",
    "신정",       "2027-01-01",
    "삼일절",     "2027-03-01",
    "어린이날",   "2027-05-05",
    "현충일",     "2027-06-06",
    "광복절",     "2027-08-15",
    "개천절",     "2027-10-03",
    "한글날",     "2027-10-09",
    "성탄절",     "2027-12-25"
  )

  lunar_holidays <- tibble::tribble(
    ~holiday,         ~ds,
    "설날연휴",       "2026-02-16",
    "설날",           "2026-02-17",
    "설날연휴",       "2026-02-18",
    "부처님오신날",   "2026-05-24",
    "추석연휴",       "2026-10-04",
    "추석",           "2026-10-05",
    "추석연휴",       "2026-10-06",
    "설날연휴",       "2027-02-05",
    "설날",           "2027-02-06",
    "설날연휴",       "2027-02-07",
    "부처님오신날",   "2027-05-13",
    "추석연휴",       "2027-09-24",
    "추석",           "2027-09-25",
    "추석연휴",       "2027-09-26"
  )

  substitute_holidays <- tibble::tribble(
    ~holiday,         ~ds,
    "삼일절대체",     "2026-03-02",
    "어린이날대체",   "2027-05-06",
    "한글날대체",     "2027-10-11"
  )

  bind_rows(fixed_holidays, lunar_holidays, substitute_holidays) %>%
    mutate(ds = as.Date(.data$ds))
}

# =============================================================================
# 2. 주차별 평균 계산 함수
# =============================================================================

calculate_weekly_avg <- function(data, value_col, type_label) {
  data %>%
    mutate(
      year_week = floor_date(date, "week", week_start = 1)  # 월요일 시작
    ) %>%
    group_by(.data$year_week) %>%
    summarise(
      가격 = mean(!!sym(value_col), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(.data$가격)) %>%
    mutate(유형 = type_label)
}

# =============================================================================
# 3. 데이터 전처리 - 주차별 평균
# =============================================================================

# 최근 6개월 실제 데이터 (주차별 평균)
six_months_ago <- max(actual_data$date) - 183

# 향후 6개월 예측 데이터만 사용
forecast_six_months <- forecast_data %>%
  filter(date <= min(date) + 183)

actual_weekly <- list()
for (var in c("암송아지", "숫송아지", "지육_평균")) {
  actual_weekly[[var]] <- actual_data %>%
    filter(date >= six_months_ago) %>%
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
  forecast_weekly[[var]] <- forecast_six_months %>%
    calculate_weekly_avg(pred_col, "예측") %>%
    mutate(변수 = var)
  
  # 신뢰구간 주차별 평균
  forecast_ci_weekly[[var]] <- forecast_six_months %>%
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
# 4. 그래프 생성 함수
# =============================================================================

create_combined_calf_plot <- function() {
  calf_levels <- c("암송아지", "숫송아지")

  actual_var <- actual_weekly_all %>%
    filter(.data$변수 %in% calf_levels)

  forecast_var <- forecast_weekly_all %>%
    filter(.data$변수 %in% calf_levels)

  ci_var <- forecast_ci_all %>%
    filter(.data$변수 %in% calf_levels)

  ggplot() +
    geom_ribbon(
      data = ci_var,
      aes(
        x = .data$year_week,
        ymin = .data$하한,
        ymax = .data$상한,
        fill = .data$변수
      ),
      alpha = 0.12,
      show.legend = FALSE
    ) +
    geom_line(
      data = actual_var,
      aes(x = .data$year_week, y = .data$가격, color = .data$변수),
      linewidth = 1.2
    ) +
    geom_point(
      data = actual_var,
      aes(x = .data$year_week, y = .data$가격, color = .data$변수),
      size = 2.3
    ) +
    geom_line(
      data = forecast_var,
      aes(x = .data$year_week, y = .data$가격, color = .data$변수),
      linewidth = 1.2,
      linetype = "dashed"
    ) +
    geom_point(
      data = forecast_var,
      aes(x = .data$year_week, y = .data$가격, color = .data$변수),
      size = 1.4,
      alpha = 0.7
    ) +
    geom_vline(
      xintercept = min(forecast_var$year_week),
      linetype = "dotted",
      color = "#e74c3c",
      linewidth = 0.8
    ) +
    labs(
      title = "송아지 가격 예측",
      subtitle = paste0(
        "최근 6개월 실제 + 향후 6개월 예측 | 실선: 실제, 점선: 예측"
      ),
      x = "주차",
      y = "가격 (원)",
      color = NULL,
      caption = "음영은 80% 신뢰구간"
    ) +
    theme_minimal(base_family = korean_font) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10),
      plot.caption = element_text(color = "gray50", size = 8),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    ) +
    scale_color_manual(values = c("암송아지" = "#c0392b", "숫송아지" = "#2980b9")) +
    scale_fill_manual(values = c("암송아지" = "#c0392b", "숫송아지" = "#2980b9")) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma)
}

create_weekly_forecast_plot <- function(var_name, y_label, title_suffix) {
  
  # 실제 데이터 필터
  actual_var <- actual_weekly_all %>% filter(.data$변수 == var_name)
  
  # 예측 데이터 필터
  forecast_var <- forecast_weekly_all %>% filter(.data$변수 == var_name)
  
  # 신뢰구간 필터
  ci_var <- forecast_ci_all %>% filter(.data$변수 == var_name)
  
  # 연결점 (마지막 실제 + 첫 예측)
  last_actual <- actual_var %>% filter(.data$year_week == max(.data$year_week))
  first_forecast <- forecast_var %>% filter(.data$year_week == min(.data$year_week))
  
  # 그래프 생성
  p <- ggplot() +
    # 신뢰구간 (80%)
    geom_ribbon(data = ci_var, aes(x = .data$year_week, ymin = .data$하한, ymax = .data$상한),
                fill = "#3498db", alpha = 0.2) +
    # 실제 가격 선
    geom_line(data = actual_var, aes(x = .data$year_week, y = .data$가격), 
              color = "#2c3e50", linewidth = 1.2) +
    # 실제 가격 점
    geom_point(data = actual_var, aes(x = .data$year_week, y = .data$가격), 
               color = "#2c3e50", size = 2.5) +
    # 연결선 (실제 → 예측)
    geom_segment(aes(x = last_actual$year_week, xend = first_forecast$year_week,
                     y = last_actual$가격, yend = first_forecast$가격),
                 color = "#3498db", linetype = "dashed", linewidth = 1) +
    # 예측 가격 선
    geom_line(data = forecast_var, aes(x = .data$year_week, y = .data$가격), 
              color = "#3498db", linewidth = 1.2) +
    # 예측 가격 점
    geom_point(data = forecast_var, aes(x = .data$year_week, y = .data$가격), 
               color = "#3498db", size = 1.5, alpha = 0.7) +
    # 예측 시작 수직선
    geom_vline(xintercept = min(forecast_var$year_week), 
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
    theme_minimal(base_family = korean_font) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40", size = 10),
      plot.caption = element_text(color = "gray50", size = 8),
      axis.text = element_text(size = 9),
      panel.grid.minor = element_blank()
    ) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma)
  
  return(p)
}

# =============================================================================
# 5. Prophet 구성요소 그래프 생성
# =============================================================================

create_component_plot <- function() {
  holidays <- create_korean_holidays()

  model_data <- actual_data %>%
    filter(.data$date > as.Date("2018-01-01")) %>%
    filter(.data$wday %in% c("Tue", "Wed", "Thu", "Fri")) %>%
    transmute(
      ds = .data$date,
      y = .data$지육_평균
    ) %>%
    filter(!is.na(.data$y))

  model <- prophet(
    holidays = holidays,
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "multiplicative",
    interval.width = 0.80,
    changepoint.prior.scale = 0.5,
    seasonality.prior.scale = 10,
    holidays.prior.scale    = 1
  )

  model <- fit.prophet(model, model_data)

  component_dates <- tibble(
    ds = seq(as.Date("2026-01-01"), as.Date("2026-12-31"), by = "day")
  )

  component_forecast <- predict(model, component_dates)

  trend_plot <- component_forecast %>%
    transmute(ds = as.Date(.data$ds), effect = .data$trend) %>%
    ggplot(aes(x = .data$ds, y = .data$effect)) +
    geom_line(color = "#2c3e50", linewidth = 1) +
    labs(title = "추세(Trend)", x = NULL, y = "가격 (원/kg)") +
    theme_minimal(base_family = korean_font) +
    theme(plot.title = element_text(face = "bold", size = 12)) +
    scale_x_date(date_labels = "%m", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma)

  annual_plot <- component_forecast %>%
    transmute(ds = as.Date(.data$ds), effect = .data$yearly) %>%
    ggplot(aes(x = .data$ds, y = .data$effect)) +
    geom_hline(yintercept = 0, color = "#b2bec3", linewidth = 0.6) +
    geom_line(color = "#16a085", linewidth = 1) +
    labs(title = "연간 효과", x = NULL, y = "기여도") +
    theme_minimal(base_family = korean_font) +
    theme(plot.title = element_text(face = "bold", size = 12)) +
    scale_x_date(date_labels = "%m", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma)

  holiday_effects <- create_korean_holidays() %>%
    filter(.data$ds >= as.Date("2026-01-01"), .data$ds <= as.Date("2026-12-31")) %>%
    distinct(.data$holiday, .data$ds) %>%
    left_join(
      component_forecast %>% transmute(ds = as.Date(.data$ds), effect = .data$holidays),
      by = "ds"
    )

  holiday_plot <- holiday_effects %>%
    ggplot(aes(x = .data$ds, y = .data$effect)) +
    geom_hline(yintercept = 0, color = "#b2bec3", linewidth = 0.6) +
    geom_col(fill = "#8e44ad", alpha = 0.6, width = 5) +
    geom_point(color = "#8e44ad", size = 2) +
    geom_text(aes(label = .data$holiday), angle = 90, vjust = -0.3, hjust = 0, size = 2.7, check_overlap = TRUE) +
    labs(title = "공휴일 효과", x = NULL, y = "기여도") +
    theme_minimal(base_family = korean_font) +
    theme(plot.title = element_text(face = "bold", size = 12)) +
    scale_x_date(date_labels = "%y-%m", date_breaks = "2 months") +
    scale_y_continuous(labels = scales::comma)

  trend_plot / annual_plot / holiday_plot +
    plot_annotation(
      title = "지육_평균 Prophet 모델 구성요소",
      subtitle = "추세 · 연간 계절성 · 공휴일 효과 분해 (화~금 데이터만 사용 → 요일 효과 미도입)",
      theme = theme(
        plot.title = element_text(face = "bold", size = 15, family = korean_font),
        plot.subtitle = element_text(size = 10, color = "gray40", family = korean_font)
      )
    )
}

# =============================================================================
# 6. 메인 그래프 생성 및 저장
# =============================================================================

# 개별 그래프 생성
p1 <- create_combined_calf_plot()
p2 <- create_weekly_forecast_plot("지육_평균", "가격 (원/kg)", "지육 평균")

combined_plot <- p1 / p2 +
  plot_annotation(
    title = "한우 가격 예측 (Prophet 모델) - 주차별 평균",
    subtitle = paste0("생성일: ", Sys.Date(), " | 실제 6개월 + 예측 6개월 (화~금만 사용, 월요일은 일요일 휴장으로 인한 물량 부족으로 제외)"),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, family = korean_font),
      plot.subtitle = element_text(color = "gray50", size = 11, family = korean_font)
    )
  )

# 구성요소 그래프 생성
component_plot <- create_component_plot()

# =============================================================================
# 7. 저장
# =============================================================================

ggsave("assets/forecast-plot.png", combined_plot, 
  width = 12, height = 10, dpi = 150, bg = "white")

ggsave("assets/model-components.png", component_plot,
  width = 12, height = 9, dpi = 150, bg = "white")

cat("그래프 저장 완료: assets/forecast-plot.png\n")
cat("구성요소 그래프 저장 완료: assets/model-components.png\n")
