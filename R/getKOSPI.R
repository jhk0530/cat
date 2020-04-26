#' @title Get recent KOSPI Index value
#' @description description
#' @usage getKOSPI()
#' @return data.frame
#' @import rvest
#' @import xml2
#' @export
#' @seealso getKOSPI200

getKOSPI <- function(){
  url_index <- 'https://finance.naver.com/sise/sise_index.nhn?code=KOSPI'

  indexPage <- read_html(url_index, encoding = 'CP949')

  # daily index
  idx <- indexPage %>%
    html_node('#now_value') %>%
    html_text() %>%
    gsub(pattern = ',', replacement = '') %>%
    as.numeric()

  # daily rate
  rate <- indexPage %>%
    html_node('#change_value_and_rate') %>%
    html_text() %>%
    gsub(pattern = '상승',replacement = '') %>%
    gsub(pattern = '하락',replacement = '') %>%
    gsub(pattern = '%',replacement = '') %>%
    strsplit(split = ' ') %>%
    unlist() %>%
    as.numeric()

  if(rate[2] < 0){ rate[1] = 0-rate[1] }

  # daily trend by group
  vals <- indexPage %>%
    html_node('.lst_kos_info') %>%
    html_text() %>%
    gsub(pattern = '개인', replacement = '') %>%
    gsub(pattern = '외국인', replacement = '') %>%
    gsub(pattern = '기관', replacement = '') %>%
    gsub(pattern = ' ', replacement = '') %>%
    gsub(pattern = '억', replacement = '') %>%
    gsub(pattern = ',', replacement = '') %>%
    strsplit(split = '\n\t') %>%
    unlist()
  vals <- c(idx, rate,  as.numeric(vals[2:4]))
  names(vals) = c("KOSPI", '전일비', '등락률', "개인 (억)", "외국인 (억)", "기관 (억)")
  return(vals)
}
