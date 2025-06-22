## 소분류 주소, html로 저장

library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(glue)
library(readr)

# 0. 셀레니움 연결
remDr = rsDriver(remoteServerAddr = "localhost",
                 browser = "chrome",
                 geckover = NULL,
                 iedrver = NULL,
                 chromever = NULL,
                 phantomver = NULL,
                 port = 4423L)
remDr_client <- remDr$client
remDr_client$open()


# 1. IT 모바일 대분류 접속
base_url <- "https://www.yes24.com/product/category/display/001001003"
remDr_client$navigate(base_url)
Sys.sleep(3)

# 2. 소분류 텍스트만 먼저 저장
initial_nodes <- remDr_client$findElements(using = "css selector", value = "li#category001001003 ul.subDpt a.lnk_cate")
subcategory_names <- sapply(initial_nodes, function(x) {
  em <- x$findChildElement(using = "css selector", value = "em.txt")
  em$getElementText()[[1]]
})

# 3. 소분류 순회
for (i in seq_along(subcategory_names)) {
  tryCatch({
    name <- subcategory_names[i]
    sub_name <- str_replace_all(name, "[^가-힣a-zA-Z0-9]", "_")
    message(glue("🔍 {i}/{length(subcategory_names)}: {name} 처리 중..."))
    
    # 👉 항상 새로 로딩
    remDr_client$navigate(base_url)
    Sys.sleep(3)
    
    # 👉 노드 다시 수집
    nodes <- remDr_client$findElements(using = "css selector", value = "li#category001001003 ul.subDpt a.lnk_cate")
    
    # 👉 이름 기준으로 해당 노드 찾기
    matched_index <- NA
    for (j in seq_along(nodes)) {
      em_node <- nodes[[j]]$findChildElement(using = "css selector", value = "em.txt")
      txt <- em_node$getElementText()[[1]]
      if (txt == name) {
        matched_index <- j
        break
      }
    }
    
    if (is.na(matched_index)) {
      warning(glue("❌ 소분류 '{name}' 노드를 찾지 못했습니다"))
      next
    }
    
    # 👉 클릭
    nodes[[matched_index]]$clickElement()
    Sys.sleep(3)
    
    # 👉 신상품순 정렬
    try({
      sort_recent <- remDr_client$findElement(using = "css selector", value = "a.sort_gb[data-search-value='RECENT']")
      sort_recent$clickElement()
      Sys.sleep(2)
    }, silent = TRUE)
    
    # 👉 120개 보기
    try({
      page_size_select <- remDr_client$findElement(using = "css selector", value = "select#pg_size")
      page_size_select$sendKeysToElement(list("120개 보기"))
      Sys.sleep(3)
    }, silent = TRUE)
    
    # 📦 링크 수집 + HTML 저장
    all_links <- c()
    page_count <- 1
    
    repeat {
      page_source <- remDr_client$getPageSource()[[1]]
      page_html <- read_html(page_source)
      
      # 링크 수집
      new_links <- page_html %>%
        html_nodes(".itemUnit .info_name .gd_name") %>%
        html_attr("href") %>%
        paste0("https://www.yes24.com", .)
      
      all_links <- c(all_links, new_links)
      
      # HTML 저장
      html_filename <- glue("yes24_html_{sub_name}_p{page_count}.html")
      writeLines(page_source, html_filename)
      message(glue("📝 HTML 저장 완료: {html_filename}"))
      page_count <- page_count + 1
      
      # 다음 페이지 이동
      current_page_node <- remDr_client$findElement(using = "css selector", value = "div.yesUI_pagen strong.num")
      current_page <- as.numeric(current_page_node$getElementText()[[1]])
      
      next_page_links <- remDr_client$findElements(using = "css selector", value = "div.yesUI_pagen a.num")
      
      next_found <- FALSE
      for (link in next_page_links) {
        page_num <- as.numeric(link$getElementAttribute("title")[[1]])
        if (!is.na(page_num) && page_num == current_page + 1) {
          link$clickElement()
          Sys.sleep(3)
          next_found <- TRUE
          break
        }
      }
      
      if (!next_found) {
        next_block <- tryCatch({
          remDr_client$findElement(using = "css selector", value = "div.yesUI_pagen a.next")
        }, error = function(e) NULL)
        
        if (!is.null(next_block)) {
          next_block$clickElement()
          Sys.sleep(3)
        } else {
          cat(glue("✅ [{name}] 모든 페이지 수집 완료\n"))
          break
        }
      }
    }
    
    # 저장
    link_df <- tibble(소분류 = name, 링크 = unique(all_links))
    write_csv(link_df, glue("yes24_links_{sub_name}.csv"))
    message(glue("📁 링크 저장 완료: yes24_links_{sub_name}.csv"))
    
  }, error = function(e) {
    message(glue("⚠️ [{subcategory_names[i]}] 에러 발생: {e$message}"))
  })
}

# 종료
remDr_client$close()
remDr$server$stop()