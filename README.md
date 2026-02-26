# EKAPE Parsing - 한우 가격 데이터 자동 수집

이 저장소는 [축산물품질평가원(EKAPE)](https://www.ekapepia.com/)에서 한우 가격 데이터를 자동으로 수집하여 Google Sheets에 저장하는 스크립트입니다.

## 🔧 설정 방법

### 1. Google Cloud 서비스 계정 설정

1. [Google Cloud Console](https://console.cloud.google.com/)에 접속
2. 새 프로젝트 생성 또는 기존 프로젝트 선택
3. **API 및 서비스** > **라이브러리**에서 다음 API 활성화:
   - Google Sheets API
   - Google Drive API
4. **API 및 서비스** > **사용자 인증 정보** > **사용자 인증 정보 만들기** > **서비스 계정**
5. 서비스 계정 생성 후, **키 관리** > **키 추가** > **새 키 만들기** > **JSON** 선택
6. 다운로드된 JSON 파일의 내용을 복사

### 2. Google Sheets 권한 설정

1. 사용할 Google Sheets 문서를 열기
2. **공유** 버튼 클릭
3. 서비스 계정 이메일 주소 추가 (예: `your-service@your-project.iam.gserviceaccount.com`)
4. **편집자** 권한 부여

### 3. GitHub Secrets 설정

1. GitHub 저장소의 **Settings** > **Secrets and variables** > **Actions**
2. **New repository secret** 클릭
3. Name: `GOOGLE_SERVICE_ACCOUNT_JSON`
4. Value: 위에서 복사한 JSON 파일 내용 전체를 붙여넣기

## 📅 자동 실행

- **스케줄**: 매일 오전 9시 (KST) 자동 실행
- **수동 실행**: Actions 탭에서 "Run workflow" 버튼으로 수동 실행 가능

## 📁 파일 구조

```
├── .github/
│   └── workflows/
│       └── ekape-parsing.yml    # GitHub Actions 워크플로우
├── data_hanwoo_stock/           # 저장된 데이터 (xlsx)
├── ekape-parsingscript.R        # 로컬 실행용 스크립트
├── ekape-parsingscript-action.R # GitHub Actions용 스크립트
└── README.md
```

## 📊 수집 데이터

- 암송아지 가격
- 숫송아지 가격
- 농가수취가격 (600kg)
- 지육 평균가
- 지육 1등급가
- 도매 등심 1등급
- 소비자 등심 1등급
