library(future)
library(future.apply)
library(stringr)
library(readr)
library(glue)

plan(multisession, workers = 3)

csv_files <- list.files(pattern = "^yes24_links_.*\\.csv$")

future_lapply(seq_along(csv_files), future.seed = TRUE, function(idx) {
  library(RSelenium)
  library(readr)
  library(stringr)
  library(glue)
  library(rvest)
  library(xml2)
  
  csv_file <- csv_files[idx]
  sub_name <- str_remove_all(csv_file, "^yes24_links_|\\.csv$")
  safe_sub_name <- str_replace_all(sub_name, "[^가-힣a-zA-Z0-9]+", "_")
  port <- 5000L + idx
  
  message(glue("🚀 [{idx}_{sub_name}] 시작 (포트 {port})"))
  
  # 셀레니움 연결
  remDr = rsDriver(remoteServerAddr = "localhost",
                   browser = "chrome",
                   geckover = NULL,
                   iedrver = NULL,
                   chromever = NULL,
                   phantomver = NULL,
                   port = port)
  remDr_client <- remDr$client
  
  links <- read_csv(csv_file)$링크
  total <- length(links)
  folder_index <- 1
  file_count <- 0
  
  for (i in seq_along(links)) {
    folder_path <- glue("html/{safe_sub_name}_{sprintf('%03d', folder_index)}")
    if (!dir.exists(folder_path)) {
      dir.create(folder_path, recursive = TRUE)
      message(glue("📁 폴더 생성됨: {folder_path}"))
    }
    
    prod_id <- str_extract(links[i], "\\d+$")
    html_filename <- glue("{folder_path}/{prod_id}.html")
    
    tryCatch({
      remDr_client$navigate(links[i])
      Sys.sleep(3)
      
      expand_btns <- remDr_client$findElements(using = "css selector", value = "div.btn_halfMore a")
      for (btn in expand_btns) {
        try(btn$clickElement(), silent = TRUE)
        Sys.sleep(1)
      }
      
      # 원본 HTML 저장
      page_source <- remDr_client$getPageSource()[[1]]
      writeLines(page_source, html_filename)
      
      # ✅ 저장 직후 후처리: <script> 제거
      html <- read_html(html_filename)
      html %>%
        xml_find_all(".//script") %>%
        xml_remove()
      writeLines(as.character(html), html_filename)
      
      file_count <- file_count + 1
      message(glue("✅ [{safe_sub_name}] {i}/{total} 저장 및 후처리 완료: {html_filename}"))
      
      if (file_count %% 120 == 0) {
        folder_index <- folder_index + 1
      }
      
    }, error = function(e) {
      message(glue("⚠️ [{safe_sub_name}] {i} 실패: {e$message}"))
    })
  }
  
  remDr_client$close()
  remDr$server$stop()
  message(glue("🏁 [{safe_sub_name}] 완료"))
})



