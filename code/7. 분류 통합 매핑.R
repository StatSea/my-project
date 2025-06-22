# 데이터 불러오기
books <- read_csv("C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/csv 모음 완/books_with_publishers_filtered.csv")


# 매핑 테이블 생성
field_mapping <- tribble(
  ~field_id, ~new_field_id, ~new_category,
  1,         2,             "디자인/미디어/게임",
  2,         2,             "디자인/미디어/게임",
  3,         3,             "AI/데이터/보안",
  4,         1,             "프로그래밍/개발",
  5,         5,             "IT 비즈니스/활용",
  6,         4,             "컴퓨터 활용/입문",
  7,         1,             "프로그래밍/개발",
  8,         3,             "AI/데이터/보안",
  9,         5,             "IT 비즈니스/활용",
  10,        6,             "컴퓨터 공학/수험",
  11,        6,             "컴퓨터 공학/수험",
  12,        4,             "컴퓨터 활용/입문",
  13,        1,             "프로그래밍/개발",
  14,        1,             "프로그래밍/개발"
)

# 데이터프레임과 매핑 결합
books <- books %>%
  left_join(field_mapping, by = "field_id")

# 결과 확인
books %>% select(field_id, new_field_id, new_category, 책제목)

head(books)
