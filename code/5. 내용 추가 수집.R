library(rvest)
library(dplyr)
library(stringr)
library(tidyr)
library(furrr)
library(progressr)

plan(multisession, workers = parallel::detectCores())
handlers("txtprogressbar")

# 경로 설정
base_path <- "C:/Users/sg897/OneDrive/문서/IT 모바일/소분류"

# 소분류별 field_id 매핑
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

# 출판사만 추출하는 HTML 파싱 함수
parse_publisher_only <- function(file) {
  page <- tryCatch(read_html(file), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)
  
  pub <- page %>% html_element(".gd_pub a") %>%
    html_text(trim = TRUE)
  
  tibble(
    source_file = basename(file),
    publisher = pub %||% NA_character_
  )
}

# 소분류별 순회
for (category in names(fields)) {
  html_root <- file.path(base_path, category, "책 html")
  if (!dir.exists(html_root)) next
  
  html_files <- list.files(html_root, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  if (length(html_files) == 0) next
  
  message("📂 ", category, " - 출판사 수집 중 (", length(html_files), "개)...")
  
  publishers <- with_progress({
    p <- progressor(along = html_files)
    future_map_dfr(html_files, ~{
      result <- parse_publisher_only(.x)
      p()
      result
    })
  })
  
  # 저장
  clean_name <- str_replace_all(category, "[^가-힣a-zA-Z0-9_]+", "_")
  save_dir <- file.path(base_path, category)
  out_file <- file.path(save_dir, paste0("publishers_", clean_name, ".csv"))
  write.csv(publishers, out_file, row.names = FALSE, fileEncoding = "utf-8")
  message("✅ 저장 완료: ", out_file)
}

