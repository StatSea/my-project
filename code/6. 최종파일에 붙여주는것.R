library(readr)
library(dplyr)
library(stringr)

# 1. 원본 책 데이터 불러오기
books_all <- read_csv("C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/csv 모음 완/books_all_combined.csv")

# 2. field_id가 있는 열이 없으면 만들어줌 (예: books_all$source_file에서 파생)
# 예시: source_file이 "123456789_3.html" 형태라면 아래처럼 추출
# books_all <- books_all %>% mutate(field_id = str_extract(source_file, "(?<=_)[0-9]+(?=\\.html)"))

# 여긴 예시 없이 field_id가 있다고 가정

# 3. 모든 publishers 파일 경로 생성
base_path <- "C:/Users/sg897/OneDrive/문서/csv 모음/"
file_names <- paste0("publishers_", 1:14, "_", c("게임", "그래픽_디자인_멀티미디어", "네트워크_해킹_보안",
                                                 "모바일_프로그래밍", "모바일_태블릿_sns", "오피스_활용",
                                                 "웹사이트", "인공지능", "인터넷_비즈니스", "컴퓨터_공학",
                                                 "컴퓨터_수험서", "컴퓨터_입문_활용", "프로그래밍_언어", "os_데이터_베이스"), ".csv")
full_paths <- paste0(base_path, file_names)

# 4. 모든 publishers 불러오며 field_id 열 추가
publishers_all <- bind_rows(lapply(1:14, function(i) {
  df <- read_csv(full_paths[i])
  df$field_id <- i
  return(df)
}))

# 5. books_all과 source_file + field_id 기준으로 병합
books_merged <- books_all %>%
  left_join(publishers_all, by = c("source_file", "field_id"))

# 6. 저장
write_csv(books_merged, "C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/csv 모음 완/books_with_publishers_filtered.csv")
