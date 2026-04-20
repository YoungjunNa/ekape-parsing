# EKAPE Parsing - 한우 가격 데이터 자동 수집 및 예측

[축산물품질평가원(EKAPE)](https://www.ekapepia.com/)에서 한우 가격 데이터를 자동으로 수집하고, **Prophet 모델**을 활용하여 3개월 가격을 예측합니다.

## 📈 가격 예측 그래프

> 최근 3개월 실제 가격 + 1년 예측값 (**주차별 평균**, 80% 신뢰구간)

![한우 가격 예측](assets/forecast-plot.png)

## 📅 자동 실행

- **스케줄**: 매주 월~금 오전 9시 (KST) 자동 실행
- **수동 실행**: Actions 탭에서 "Run workflow" 버튼으로 수동 실행 가능
- **실행 내용**: 데이터 수집 → Prophet 예측 → CSV 저장 → 자동 커밋

## 📊 수집 데이터

| 항목 | 설명 |
|------|------|
| date | 날짜 |
| 암송아지 | 암송아지 가격 (원) |
| 숫송아지 | 숫송아지 가격 (원) |
| 농가수취가격_600kg | 농가수취가격 600kg 기준 (원) |
| 지육_평균 | 지육 평균가 (원/kg) |
| 지육_1등급 | 지육 1등급가 (원/kg) |
| 도매_등심1등급 | 도매 등심 1등급 (원/kg) |
| 소비자_등심1등급 | 소비자 등심 1등급 (원/kg) |

## 🔮 예측 데이터

Prophet 모델을 사용하여 아래 3가지 변수에 대해 **1년 (약 260일, 평일만)** 예측을 수행합니다.

| 항목 | 설명 |
|------|------|
| 암송아지_예측 | 암송아지 가격 예측값 (원) |
| 암송아지_하한/상한 | 80% 신뢰구간 |
| 숫송아지_예측 | 숫송아지 가격 예측값 (원) |
| 숫송아지_하한/상한 | 80% 신뢰구간 |
| 지육_평균_예측 | 지육 평균가 예측값 (원/kg) |
| 지육_평균_하한/상한 | 80% 신뢰구간 |

### 예측 모델 설정
- **패키지**: [Prophet](https://facebook.github.io/prophet/) (Facebook/Meta)
- **학습 데이터**: 2018년 이후, 화수목금만 (주말/월요일 제외)
- **화요일 효과**: 쉬는 날 다음날(화요일) 가격 하락 패턴 반영 (regressor)
- **주간 계절성**: 활성화 (평일 데이터 패턴 반영)
- **연간 계절성**: 활성화 (장기 트렌드 반영)
- **공휴일 반영**: 한국 공휴일 (설날, 추석, 어린이날 등)
- **예측 출력**: 화~금요일만 (주말/공휴일 제외)

## 📥 데이터 사용 방법

### 실제 가격 데이터

```r
# R
library(readr)
actual <- read_csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-stock.csv")
head(actual)
```

```python
# Python
import pandas as pd
actual = pd.read_csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-stock.csv")
actual.head()
```

### 예측 데이터

```r
# R
library(readr)
forecast <- read_csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-forecast.csv")

# 암송아지 예측값과 신뢰구간 확인
forecast |> 
  select(date, 암송아지_예측, 암송아지_하한, 암송아지_상한) |>
  head(10)
```

```python
# Python
import pandas as pd
forecast = pd.read_csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-forecast.csv")

# 지육 평균 예측값 확인
forecast[['date', '지육_평균_예측', '지육_평균_하한', '지육_평균_상한']].head(10)
```

### 실제 + 예측 데이터 결합 예시

```r
# R: 실제 데이터와 예측 데이터 결합
library(dplyr)
library(readr)

actual <- read_csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-stock.csv")
forecast <- read_csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-forecast.csv")

# 암송아지 데이터 결합
combined <- bind_rows(
  actual |> select(date, 암송아지) |> mutate(유형 = "실제"),
  forecast |> select(date, 암송아지 = 암송아지_예측) |> mutate(유형 = "예측")
)
```

## 📁 파일 구조

```
├── .github/
│   └── workflows/
│       └── ekape-parsing.yml      # GitHub Actions 워크플로우
├── assets/
│   └── forecast-plot.png          # 예측 그래프 이미지
├── data_hanwoo_stock/
│   ├── hanwoo-stock.csv           # 실제 가격 데이터
│   └── hanwoo-forecast.csv        # 예측 데이터 (90일)
├── ekape-parsingscript-action.R   # 데이터 수집 스크립트
├── ekape-forecast.R               # Prophet 예측 스크립트
├── generate-plot.R                # 시각화 생성 스크립트
└── README.md
```

## 🛠️ 로컬 실행

```bash
# 1. 데이터 수집
Rscript ekape-parsingscript-action.R

# 2. 가격 예측
Rscript ekape-forecast.R

# 3. 그래프 생성 (선택)
Rscript generate-plot.R
```

## 📜 라이선스

이 프로젝트는 MIT 라이선스를 따릅니다. 데이터 출처: [축산물품질평가원(EKAPE)](https://www.ekapepia.com/)
