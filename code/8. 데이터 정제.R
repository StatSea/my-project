library(ggplot2)
library(readr)
library(dplyr)
library(scales)

books_원본 <- read_csv("C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/csv 모음 완/books_cleaned.csv")
books <- read_csv("C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/csv 모음 완/books_cleaned.csv")

###############################################################


1. 데이터 정제


###############################################################

# 쪽수 시각화
ggplot(books_원본, aes(x = 쪽수)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +  # 50쪽 단위 구간화
  scale_x_continuous(labels = scales::comma) +
  labs(title = "쪽수별 책 개수 분포", x = "쪽수 (페이지)", y = "책 개수") +
  theme_minimal()

# 출판연도 추가
books <- books %>%
  mutate(출판연도 = year(ymd(출간일)))

books_원본 <- books_원본 %>%
  mutate(출판연도 = year(ymd(출간일)))

# 쪽수가 na인 경우는 제외 (세트여서 쪽수가 없거나 너무 오래된 책이라 정보가 없기 때문)
books <- books %>%
  filter(!is.na(쪽수)) %>%       # 쪽수 없는 데이터 제외 (이미 있음)
  filter(쪽수 > 1) %>%           # 쪽수 1 이하 (USB, CD, DVD) 제외 (이미 있음)
  filter(sales_id == 1) %>%      # 판매중 데이터만 남기기
  filter(!is.na(판매가), !is.na(출간일))  # 판매가/출간일 없는 데이터 제외


summary(books)


# 페이지 당 가격 확인
books <- books %>%
  mutate(페이지당_가격 = 판매가 / 쪽수)

summary(books)

# 1️⃣ IQR 계산
Q1 <- quantile(books$페이지당_가격, 0.25, na.rm = TRUE)
Q3 <- quantile(books$페이지당_가격, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# 2️⃣ 이상치 기준
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# 3️⃣ 이상치 제거 데이터 생성
books<- books %>%
  filter(페이지당_가격 >= lower_bound, 페이지당_가격 <= upper_bound)


library(tidyr)
library(dplyr)
library(ggplot2)

# 1️⃣ 요약 데이터 생성
books_summary <- books %>%
  group_by(출판연도) %>%
  summarise(
    책_개수 = n(),
    페이지당_가격_평균 = mean(페이지당_가격, na.rm = TRUE),
    페이지당_가격_중앙값 = median(페이지당_가격, na.rm = TRUE)
  ) %>%
  ungroup()

# 2️⃣ 선 데이터 형태 변환 (pivot_longer)
lines_data <- books_summary %>%
  pivot_longer(
    cols = c(페이지당_가격_평균, 페이지당_가격_중앙값),
    names_to = "지표",
    values_to = "페이지당_가격"
  )

# 3️⃣ 히스토그램 + 선 그래프 그리기
ggplot() +
  geom_col(data = books_summary, aes(x = 출판연도, y = 책_개수), fill = "skyblue", alpha = 0.7) +
  geom_line(data = lines_data, aes(x = 출판연도, y = 페이지당_가격 * 20, color = 지표), size = 1) +  # *20은 축 보정용
  scale_y_continuous(
    name = "책 개수",
    sec.axis = sec_axis(~./20, name = "페이지당 가격 (원)")
  ) +
  scale_color_manual(
    values = c("페이지당_가격_평균" = "red", "페이지당_가격_중앙값" = "blue"),
    labels = c("페이지당 가격 (평균)", "페이지당 가격 (중앙값)")
  ) +
  labs(
    title = "연도별 책 개수 & 페이지당 가격 (평균/중앙값) 추세",
    x = "출판연도",
    color = "지표"
  ) +
  theme_minimal(base_size = 14)



library(dplyr)
library(tidyr)
library(ggplot2)

# 1️⃣ 2000년 이후 데이터 필터링
books_filtered <- books %>%
  filter(출판연도 >= 2000)

# 2️⃣ 연도별 요약 데이터 (책 개수 + 페이지당 가격)
books_summary <- books_filtered %>%
  group_by(출판연도) %>%
  summarise(
    책_개수 = n(),
    페이지당_가격_평균 = mean(페이지당_가격, na.rm = TRUE),
    페이지당_가격_중앙값 = median(페이지당_가격, na.rm = TRUE)
  ) %>%
  ungroup()

# 3️⃣ 페이지당 가격 선그래프용 데이터 변환 (pivot_longer)
lines_data <- books_summary %>%
  pivot_longer(
    cols = c(페이지당_가격_평균, 페이지당_가격_중앙값),
    names_to = "지표",
    values_to = "페이지당_가격"
  )

# 4️⃣ 책 개수 + 페이지당 가격 그래프 그리기
ggplot() +
  geom_col(data = books_summary, aes(x = 출판연도, y = 책_개수), fill = "skyblue", alpha = 0.7) +
  geom_line(data = lines_data, aes(x = 출판연도, y = 페이지당_가격 * 20, color = 지표), size = 1) +  # *20은 축 보정
  scale_y_continuous(
    name = "책 개수",
    sec.axis = sec_axis(~./20, name = "페이지당 가격 (원)")
  ) +
  scale_color_manual(
    values = c("페이지당_가격_평균" = "blue", "페이지당_가격_중앙값" = "red"),
    labels = c("페이지당 가격 (평균)", "페이지당 가격 (중앙값)")
  ) +
  labs(
    title = "연도별 책 개수 & 페이지당 가격 (평균/중앙값) 추세",
    x = "출판연도",
    color = "지표"
  ) +
  theme_minimal(base_size = 14)


# 1️⃣ books 업데이트
books <- books_filtered

# 2️⃣ CSV 저장 (경로는 너의 작업 경로에 맞게 지정)
write.csv(books, "books_최종.csv", row.names = FALSE, fileEncoding = "UTF-8")

# 저장 경로 확인 (현재 작업 디렉토리 확인)
getwd()



books_cleaned <- books %>%
  group_by(new_category, 책제목) %>%
  slice_max(order_by = 판매지수, n = 1, with_ties = FALSE) %>%
  ungroup()

# CSV로 저장
write.csv(books_cleaned, "books_최종.csv", row.names = FALSE)




