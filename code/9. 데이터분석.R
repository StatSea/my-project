library(dplyr)
library(ggplot2)

# 필요한 라이브러리
library(dplyr)
library(ggplot2)
library(scales)

# 데이터 불러오기
books <- read.csv("C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/books_최종.csv")  # 파일 경로에 맞게 수정

options(scipen = 999)  # scipen 값 크게 주면 지수 표기 대신 일반 표기로 보여줌

ggplot(books, aes(x = 판매가, y = 판매지수)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "판매가 vs 판매지수", x = "판매가 (원)", y = "판매지수") +
  theme_minimal()

ggplot(books, aes(x = as.factor(출판연도), y = 판매지수)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(title = "출판연도별 판매지수 분포", x = "출판연도", y = "판매지수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(books, aes(x = reorder(new_category, 판매지수, FUN = median), y = 판매지수)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "카테고리별 판매지수 분포", x = "카테고리", y = "판매지수") +
  coord_flip() +
  theme_minimal()

# 판매지수 0인 책: 비인기 책
books <- books %>% mutate(판매지수 = ifelse(is.na(판매지수), 0, 판매지수))

non_popular_books <- books %>% filter(판매지수 == 0 )

# 판매지수 0이 아닌 책: 인기 책 후보
popular_books <- books %>% filter(판매지수 > 0)


# 산점도
ggplot(popular_books, aes(x = 판매가, y = 판매지수)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "판매가 vs 판매지수", x = "판매가 (원)", y = "판매지수") +
  theme_minimal()

## 판매지수가 높은 책들은 뭐지?

# 출판연도별 판매지수 분포 (박스플롯)
ggplot(popular_books, aes(x = as.factor(출판연도), y = 판매지수)) +
  geom_boxplot(fill = "tomato", outlier.color = "red") +
  labs(title = "출판연도별 판매지수 분포", x = "출판연도", y = "판매지수") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 카테고리별 판매지수 분포 (박스플롯)
ggplot(popular_books, aes(x = reorder(new_category, 판매지수, FUN = median), y = 판매지수)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(title = "카테고리별 판매지수 분포", x = "카테고리", y = "판매지수") +
  coord_flip() +
  theme_minimal()

# Top 10 출판사별 판매지수 중앙값
top10_publishers <- popular_books %>%
  group_by(publisher) %>%
  summarise(중앙값_판매지수 = median(판매지수, na.rm = TRUE),
            책_개수 = n()) %>%
  arrange(desc(중앙값_판매지수)) %>%
  slice(1:10)

# 시각화
ggplot(top10_publishers, aes(x = reorder(publisher, 중앙값_판매지수), y = 중앙값_판매지수)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 출판사별 중앙값 판매지수", x = "출판사", y = "중앙값 판매지수") +
  theme_minimal()

# 판매지수 vs 쪽수 (산점도)
ggplot(popular_books, aes(x = 쪽수, y = 판매지수)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "쪽수 vs 판매지수", x = "쪽수", y = "판매지수") +
  theme_minimal()

# 페이지당 가격 변수 생성
popular_books <- popular_books %>%
  mutate(페이지당_가격 = 판매가 / 쪽수)

# 판매지수 vs 페이지당 가격 (산점도)
ggplot(popular_books, aes(x = 페이지당_가격, y = 판매지수)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "페이지당 가격 vs 판매지수", x = "페이지당 가격 (원)", y = "판매지수") +
  theme_minimal()

# 카테고리별 판매지수 상위 5개 책 추출
top5_by_category_all <- popular_books %>%
  group_by(new_category) %>%
  slice_max(order_by = 판매지수, n = 10, with_ties = FALSE) %>%
  arrange(new_category, desc(판매지수)) %>%
  select(new_category, 책제목, publisher, 판매지수, 판매가, 출판연도)

# 결과 확인
top5_by_category_all

#############################


books %>%
  group_by(new_category) %>%
  summarise(책_개수 = n()) %>%
  arrange(desc(책_개수))


library(ggplot2)

ggplot(books, aes(x = reorder(new_category, 판매지수, FUN = median), y = 판매지수)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red", outlier.shape = 16, alpha = 0.6) +
  labs(title = "카테고리별 판매지수 분포", x = "카테고리", y = "판매지수") +
  coord_flip() +
  theme_minimal()

books_1<- books %>%
  filter(!is.na(판매지수) & 판매지수 != 0) %>%  # NA와 0 제거
  group_by(책제목) %>%
  mutate(중복_여부 = ifelse(n_distinct(new_category) > 1, 1, 0)) %>%
  ungroup()



# 카테고리별 판매지수 상위 20개 추출 (중복 여부 무시)
top20_by_category_all <- books_1 %>%
  group_by(new_category) %>%
  slice_max(order_by = 판매지수, n = 20, with_ties = FALSE) %>%
  ungroup()

# 그래프 그리기 (색상은 카테고리별로 구분)
ggplot(top20_by_category_all, aes(x = 판매지수, y = new_category)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  labs(title = "카테고리별 상위 20개 판매지수 (중복 여부 무관)", x = "판매지수", y = "카테고리") +
  theme_minimal()



library(ggplot2)

# 데이터프레임: 예시 (네 데이터 프레임 이름 사용)
category_count <- data.frame(
  new_category = c("프로그래밍/개발", "컴퓨터 공학/수험", "디자인/미디어/게임", 
                   "AI/데이터/보안", "컴퓨터 활용/입문", "IT 비즈니스/활용"),
  책_개수 = c(5559, 5349, 3157, 2563, 2456, 1128)
)

# 그래프 그리기
ggplot(category_count, aes(x = reorder(new_category, 책_개수), y = 책_개수)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = 책_개수), vjust = -0.5, size = 4) +  # 라벨 추가
  labs(title = "카테고리별 책 개수", x = "카테고리", y = "책 개수") +
  coord_flip() +
  theme_minimal()

####################3
library(tidytext)
library(wordcloud)
library(RColorBrewer)

# 불용어 리스트 작성
stopwords <- c("된다", "하는", "위한", "위해", "및", "by", "with", "in", 
               "개", "제", "것", "저자", "책", "이", "그", "가", "의", 
               "은", "는", "을", "를", "에", "도", "다", "로", "으로", "부터", "2025","2024","2023","배우는")

# 제목 단어 분리 + 불용어 제거
title_words <- top20_by_category_all %>%
  unnest_tokens(word, 책제목) %>%
  filter(nchar(word) > 1, !word %in% stopwords)

# 단어 빈도 계산
title_word_freq <- title_words %>%
  count(word, sort = TRUE)

# 상위 10개 단어 추출
top_words_10 <- title_word_freq %>%
  slice_max(order_by = n, n = 10)

# 워드클라우드 그리기
set.seed(123)
wordcloud(words = top_words_10$word,
          freq = top_words_10$n,
          scale = c(4, 0.5),
          colors = brewer.pal(8, "Dark2"))

library(ggplot2)

# 히스토그램 (막대그래프) 그리기
ggplot(top_words_10, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +  # 단어 개수 라벨 추가
  labs(title = "상위 10개 단어 빈도수", x = "단어", y = "빈도수") +
  coord_flip() +  # 가로 막대 그래프
  theme_minimal()


#############3

library(dplyr)
library(ggplot2)
library(scales)

# 연도별 챗GPT/AI 키워드 포함 도서 비율 계산
ai_trend <- books_1 %>%
  filter(!is.na(출판연도)) %>%
  group_by(출판연도) %>%
  summarise(
    전체_도서수 = n(),
    AI_도서수 = sum(str_detect(책제목, paste(c(  
      "챗GPT", "gpt", "GPT", "생성형 AI", "생성형 ai", "생성 AI", "생성 ai", 
      "오픈 ai", "오픈AI", "오픈에이아이", "openai", 
      "Stable Diffusion", "DALL-E", "미드저니", 
      "LLM", "Large Language Model", "파운데이션 모델", 
      "AI 모델", "AI 생성", "생성모델", 
      "AI 기술", "AI 활용", "AI 트렌드", 
      "AI 에이전트", "에이전트", "멀티모달", "AI 챗봇", "AI 서비스",
      "open ai","Copilot", "코파일럿", "DALL-E", "DALLE", "stable diffusion", "스테이블 디퓨전",
      "미드저니", "Midjourney", "Claude", "클로드", "Mistral", "미스트랄",
      "Gemini", "제미나이", "크롤라", "크롤라AI", "LLaMA", "라마", "Falcon", "팔콘",
      "Whisper", "위스퍼", "Chatsonic", "챗소닉", "Bard", "바드", "Cursor", "커서 AI", "AI", "인공지능"
    ), collapse = "|")))
  ) %>%
  mutate(AI_비율 = round((AI_도서수 / 전체_도서수) * 100, 1)) %>%
  filter(AI_도서수 > 0)  # AI 도서가 1개 이상 있는 연도만 보기

# 그래프
ggplot(ai_trend, aes(x = 출판연도, y = AI_도서수)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0(AI_도서수, "권 (", AI_비율, "%)")), 
            vjust = -0.5, size = 3.5) +
  scale_x_continuous(breaks = seq(2016, 2025, by = 1)) +  # 연도 2020~2025 표시
  labs(title = "연도별 생성형 AI 키워드 포함 도서 출간 추이 및 비율",
       x = "출판연도", y = "출간 책 개수") +
  theme_minimal()


#########################3

keywords <- c("챗GPT", "AI", "인공지능")

books_1 <- books_1 %>%
  mutate(AI_포함 = ifelse(str_detect(책제목, paste(keywords, collapse = "|")), "AI 포함", "AI 미포함"))

# 출판사별 AI 도서 비율 및 개수 계산
publisher_ai_ratio <- books_1 %>%
  filter(!is.na(publisher)) %>%
  group_by(publisher) %>%
  summarise(전체_도서수 = n(),
            AI_도서수 = sum(AI_포함 == "AI 포함")) %>%
  mutate(AI_비율 = round((AI_도서수 / 전체_도서수) * 100, 1)) %>%
  filter(전체_도서수 >= 5) %>%  # 최소 도서 수 조건 (예: 5권 이상)
  arrange(desc(AI_비율))

# 상위 10개 출판사만 보기
publisher_ai_ratio %>%
  slice_max(order_by = AI_비율, n = 15)

library(ggplot2)

ggplot(publisher_ai_ratio %>% slice_max(order_by = AI_비율, n = 10), 
       aes(x = reorder(publisher, AI_비율), y = AI_비율)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0("AI: ", AI_도서수, " / 전체: ", 전체_도서수, "권\n(", AI_비율, "%)")), 
            hjust = 1.1,  # 막대 안쪽 위치 조정
            color = "black", size = 3.5) +
  coord_flip() +
  labs(title = "출판사별 AI 키워드 도서 비율 및 개수 (상위 10개)", 
       x = "출판사", y = "AI 도서 비율 (%)") +
  theme_minimal()

##########
# 필요한 라이브러리
library(dplyr)
library(ggplot2)
library(stringr)

# 1️⃣ AI 키워드 포함 여부 컬럼 생성
keywords <- c("챗GPT", "AI", "인공지능")

books_1_filtered <- books_1 %>%
  filter(출판연도 >= 2022) %>%
  mutate(AI_포함 = ifelse(str_detect(책제목, paste(keywords, collapse = "|")), "AI 포함", "AI 미포함"))

# 2️⃣ 출판사별 AI 비율 (출판사 내부 기준)
publisher_ai_ratio_internal <- books_1_filtered %>%
  group_by(publisher) %>%
  summarise(전체_도서수 = n(),
            AI_도서수 = sum(AI_포함 == "AI 포함")) %>%
  mutate(AI_비율 = round(AI_도서수 / 전체_도서수 * 100, 1)) %>%
  arrange(desc(AI_비율))

# 3️⃣ 전체 AI 도서 중 출판사별 AI 도서 점유율 (시장 점유율)
total_ai_books <- books_1_filtered %>%
  filter(AI_포함 == "AI 포함") %>%
  count() %>%
  pull(n)

publisher_ai_share <- books_1_filtered %>%
  filter(AI_포함 == "AI 포함") %>%
  count(publisher, name = "AI_도서수") %>%
  mutate(점유율 = round(AI_도서수 / total_ai_books * 100, 1)) %>%
  arrange(desc(점유율))

# 4️⃣ 출판사별 AI 비율 + 점유율 데이터 결합 (중복 컬럼 정리)
publisher_ai_summary <- publisher_ai_ratio_internal %>%
  left_join(publisher_ai_share, by = "publisher") %>%
  rename(AI_도서수 = AI_도서수.x) %>%  # .x를 AI_도서수로 이름 변경
  select(publisher, 전체_도서수, AI_도서수, AI_비율, 점유율)


# 5️⃣ (선택) AI 도서수 기준 필터링 + 상위 출판사 추출
ai_plot_data <- publisher_ai_summary %>%
  filter(AI_도서수 >= 5) %>%                      # AI 도서수 5권 이상
  slice_max(order_by = 점유율, n = 10)             # 점유율 상위 10개 (필요에 따라 변경 가능)

# 6️⃣ 시각화
ggplot(ai_plot_data, aes(x = reorder(publisher, 점유율), y = 점유율)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(label = paste0("점유율: ", 점유율, "%\n비율: ", AI_비율, "%\nAI: ", AI_도서수, "권 / 전체: ", 전체_도서수, "권")),
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "출판사별 AI 도서 점유율 및 비율 (2022년 이후)", 
       x = "출판사", y = "AI 도서 점유율 (%)") +
  theme_minimal()


ggplot(ai_plot_data, aes(x = reorder(publisher, 점유율), y = 점유율)) +
  geom_col(fill = "skyblue") +
  geom_text(aes(y = 점유율 / 2,  # 중앙 위치
                label = paste0("점유율: ", 점유율, "% | 비율: ", AI_비율, "% | AI: ", AI_도서수, "/", 전체_도서수)),
            size = 4, color = "black") +
  coord_flip() +
  labs(title = "출판사별 AI 도서 점유율 및 비율 (2022년 이후)", 
       x = "출판사", y = "AI 도서 점유율 (%)") +
  theme_minimal()


##########################3


# 필요한 패키지
library(dplyr)
library(ggplot2)
library(stringr)

# 키워드
keywords <- c(
  "챗GPT", "gpt", "GPT", "생성형 AI", "생성형 ai", "생성 AI", "생성 ai", 
  "오픈 ai", "오픈AI", "오픈에이아이", "openai", 
  "Stable Diffusion", "DALL-E", "미드저니", 
  "LLM", "Large Language Model", "파운데이션 모델", 
  "AI 모델", "AI 생성", "생성모델", 
  "AI 기술", "AI 활용", "AI 트렌드", 
  "AI 에이전트", "에이전트", "멀티모달", "AI 챗봇", "AI 서비스",
  "open ai","Copilot", "코파일럿", "DALL-E", "DALLE", "stable diffusion", "스테이블 디퓨전",
  "미드저니", "Midjourney", "Claude", "클로드", "Mistral", "미스트랄",
  "Gemini", "제미나이", "크롤라", "크롤라AI", "LLaMA", "라마", "Falcon", "팔콘",
  "Whisper", "위스퍼", "Chatsonic", "챗소닉", "Bard", "바드", "Cursor", "커서 AI"
)


# 2020년대 이후 데이터 필터링 + 키워드 포함 여부
books_keyword <- books %>%
  filter(출판연도 >= 2020) %>%
  mutate(AI_포함 = ifelse(str_detect(책제목, paste(keywords, collapse = "|")), "AI 포함", "AI 미포함"))

# 연도별 new_category별 키워드 비율 계산
ai_summary <- books_keyword %>%
  group_by(출판연도, new_category) %>%
  summarise(전체_도서수 = n(),
            AI_도서수 = sum(AI_포함 == "AI 포함")) %>%
  mutate(AI_비율 = round(AI_도서수 / 전체_도서수 * 100, 1)) %>%
  ungroup()

# 시각화
ggplot(ai_summary, aes(x = 출판연도, y = AI_비율, fill = new_category)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(AI_비율, "%")), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  labs(title = "카테고리별 생성형 ai 키워드 포함 비율",
       x = "출판연도", y = "AI 키워드 비율 (%)", fill = "카테고리") +
  theme_minimal(base_size = 12)



