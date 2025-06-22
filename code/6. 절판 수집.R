# 필요한 패키지
library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(furrr)
library(progressr)

# 병렬 처리 설정
plan(multisession, workers = parallel::detectCores())
handlers("txtprogressbar")

# 경로 설정
base_path <- "C:/Users/sg897/OneDrive/문서/IT 모바일/소분류"

# field_id 매핑
fields <- list(
  "1. 게임" = 1,
  "2. 그래픽 디자인 멀티미디어" = 2,
  "3. 네트워크 해킹 보안" = 3,
  "4. 모바일 프로그래밍" = 4,
  "5. 모바일 태블릿 sns" = 5,
  "6. 오피스 활용" = 6,
  "7. 웹사이트" = 7,
  "8. 인공지능" = 8,
  "9. 인터넷 비즈니스" = 9,
  "10. 컴퓨터 공학" = 10,
  "11. 컴퓨터 수험서" = 11,
  "12. 컴퓨터 입문 활용" = 12,
  "13. 프로그래밍 언어" = 13,
  "14. os 데이터 베이스" = 14
)

# 판매 상태 추출 함수 (field_id 포함)
parse_sale_state <- function(file, field_id) {
  page <- tryCatch(read_html(file), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)
  
  # 판매 상태 추출 (판매중, 절판, 품절, 예약판매 등)
  sale_state <- page %>% html_element("p.gd_saleState em") %>%
    html_text(trim = TRUE)
  
  tibble(
    source_file = basename(file),
    field_id = field_id,
    sale_state = sale_state %||% NA_character_
  )
}

# 전체 데이터를 담을 리스트
all_sale_states <- list()

# 카테고리별 순회
for (category in names(fields)) {
  field_id_val <- fields[[category]]
  html_root <- file.path(base_path, category, "책 html")
  if (!dir.exists(html_root)) next
  
  html_files <- list.files(html_root, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  if (length(html_files) == 0) next
  
  message("📂 ", category, " - 판매 상태 수집 중 (", length(html_files), "개)...")
  
  sale_states <- with_progress({
    p <- progressor(along = html_files)
    future_map_dfr(html_files, ~{
      result <- parse_sale_state(.x, field_id_val)
      p()
      result
    })
  })
  
  all_sale_states[[category]] <- sale_states
}

# 하나로 합치기
final_sale_states <- bind_rows(all_sale_states)

# 전체 CSV로 저장
write.csv(final_sale_states, file.path(base_path, "전체_판매상태_데이터.csv"), row.names = FALSE, fileEncoding = "utf-8")
message("✅ 전체 CSV 저장 완료!")

# 판매 상태 데이터 불러오기
sale_states <- read.csv("C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/전체_판매상태_데이터.csv", encoding = "UTF-8")

# books 데이터프레임에 sale_state 붙이기
books <- books %>%
  left_join(sale_states, by = c("source_file", "field_id"))

# 결과 확인
head(books %>% select(source_file, field_id, sale_state))

write.csv(books, "C:/Users/sg897/OneDrive/문서/IT 모바일/소분류/books_with_sale_state.csv", row.names = FALSE, fileEncoding = "utf-8")
