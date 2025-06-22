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

# HTML 파싱 함수
parse_html_file <- function(file, field_id) {
  page <- tryCatch(read_html(file), error = function(e) return(NULL))
  if (is.null(page)) return(NULL)
  
  extract_spec_table <- function(page) {
    rows <- page %>% html_elements("#infoset_specific table tr")
    info <- list()
    for (row in rows) {
      key <- row %>% html_element("th") %>% html_text(trim = TRUE)
      val <- row %>% html_element("td") %>% html_text(trim = TRUE)
      info[[key]] <- val
    }
    info
  }
  
  extract_raw_text <- function(page, selector) {
    el <- page %>% html_element(selector)
    if (is.null(el)) return(NA_character_)
    el %>% html_text() %>% str_squish()
  }
  
  extract_author_intro <- function(page) {
    authors <- page %>% html_elements(".authorInfoGrp")
    if (length(authors) == 0) return(NA_character_)
    paste(authors %>% html_text() %>% str_squish(), collapse = "\n\n")
  }
  
  extract_category <- function(page) {
    page %>% html_element("#infoset_goodsCate") %>%
      html_text(trim = TRUE) %>%
      str_squish()
  }
  
  extract_title <- function(page) {
    page %>% html_element("h2.gd_name") %>%
      html_text(trim = TRUE)
  }
  
  extract_author <- function(page) {
    page %>% html_element(".gd_auth") %>%
      html_text(trim = TRUE) %>%
      str_squish()
  }
  
  extract_price <- function(page, label) {
    # label = "정가" or "판매가"
    rows <- page %>% html_elements(".gd_infoTb tr")
    for (row in rows) {
      th <- row %>% html_element("th")
      if (!is.null(th) && str_detect(html_text(th), label)) {
        price <- row %>% html_element("td") %>% html_text()
        return(str_extract(price, "[0-9,]+"))
      }
    }
    return(NA_character_)
  }
  
  extract_sales_rank <- function(page) {
    rank_node <- page %>% html_element(".gd_sellNum")
    if (is.null(rank_node)) return(NA_character_)
    text <- rank_node %>% html_text(trim = TRUE)
    str_extract(text, "[0-9,]+")
  }
  
  spec <- extract_spec_table(page)
  side_info <- spec[["쪽수, 무게, 크기"]] %||% ""
  parts <- str_split(side_info, "\\|")[[1]]
  
  tibble(
    field_id = field_id,
    source_file = basename(file),
    책제목 = extract_title(page),
    저자 = extract_author(page),
    출간일 = spec[["발행일"]] %||% NA,
    정가 = extract_price(page, "정가"),
    판매가 = extract_price(page, "판매가"),
    판매지수 = extract_sales_rank(page),
    쪽수 = str_remove(parts[1], "쪽") %||% NA,
    무게 = parts[2] %||% NA,
    크기 = parts[3] %||% NA,
    ISBN13 = spec[["ISBN13"]] %||% NA,
    ISBN10 = spec[["ISBN10"]] %||% NA,
    책소개 = extract_raw_text(page, "#infoset_introduce .infoWrap_txtInner"),
    목차 = extract_raw_text(page, "#infoset_toc"),
    출판사리뷰 = extract_raw_text(page, "#infoset_pubReivew"),
    저자소개 = extract_author_intro(page),
    만든이코멘트 = extract_raw_text(page, "#infoset_authorCmt .infoWrap_txt"),
    관련분류 = extract_category(page)
  )
}

# 소분류별 순회
for (category in names(fields)) {
  field_id <- fields[[category]]
  html_root <- file.path(base_path, category, "책 html")
  if (!dir.exists(html_root)) next
  
  html_files <- list.files(html_root, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  if (length(html_files) == 0) next
  
  message("📂 ", category, " - HTML ", length(html_files), "개 처리 중...")
  
  books <- with_progress({
    p <- progressor(along = html_files)
    future_map_dfr(html_files, ~{
      result <- parse_html_file(.x, field_id)
      p()
      result
    })
  })
  
  # 저장 경로 설정
  clean_name <- str_replace_all(category, "[^가-힣a-zA-Z0-9_]+", "_")
  save_dir <- file.path(base_path, category)
  out_file <- file.path(save_dir, paste0("books_", clean_name, ".csv"))
  
  write.csv(books, out_file, row.names = FALSE, fileEncoding = "utf-8")
  message("✅ 저장 완료: ", out_file)
}

