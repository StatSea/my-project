## 대분류 html로 저장

library(rvest)
library(readr)

# 1. URL 정의
url <- "https://www.yes24.com/product/category/display/001001003"

# 2. 페이지 읽기
page_html <- read_html(url)

# 3. HTML 전체 소스 저장
write_lines(as.character(page_html), "yes24_it_mobile_page.html")

cat("✅ HTML 저장 완료: yes24_it_mobile_page.html\n")

