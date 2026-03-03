# EKAPE Parsing - 한우 가격 데이터 자동 수집

[축산물품질평가원(EKAPE)](https://www.ekapepia.com/)에서 한우 가격 데이터를 자동으로 수집하여 CSV 파일로 저장하는 GitHub Actions 워크플로우입니다.

## 📅 자동 실행

- **스케줄**: 매주 월~금 오전 9시 (KST) 자동 실행
- **수동 실행**: Actions 탭에서 "Run workflow" 버튼으로 수동 실행 가능

## 📁 파일 구조

```
├── .github/
│   └── workflows/
│       └── ekape-parsing.yml    # GitHub Actions 워크플로우
├── data_hanwoo_stock/
│   └── hanwoo-stock.csv         # 수집된 데이터
├── ekape-parsingscript-action.R # 파싱 스크립트
└── README.md
```

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

## 📥 데이터 사용

```r
# R에서 데이터 읽기
df <- read.csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-stock.csv")
```

```python
# Python에서 데이터 읽기
import pandas as pd
df = pd.read_csv("https://raw.githubusercontent.com/YoungjunNa/ekape-parsing/main/data_hanwoo_stock/hanwoo-stock.csv")
```
