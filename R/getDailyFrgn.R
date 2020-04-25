#' @title Get Daily Foreign  information
#' @description description
#' @usage getDailyFrgn('035420')
#' @param code stockCode
#' @return value
#' @import rvest
#' @import dplyr
#' @import xml2
#'
#' @export

getDailyFrgn <- function(code = NULL){
  if(is.null(code)) stop("Code not given")
  url_main <- paste0('https://finance.naver.com/item/main.nhn?code=',code)
  mainPage <- read_html(url_main, encoding = 'CP949')

  vals <-mainPage %>%
    html_node(xpath = '//*[@id="content"]/div[2]/div[1]/table/tfoot/tr') %>%
    html_text() %>%
    gsub(pattern = '\t', replacement = '') %>%
    gsub(pattern = '\n', replacement = '\t') %>%
    gsub(pattern = ',', replacement = '') %>%
    strsplit(split = '\t') %>%
    unlist()
  vals <- vals[-1]
  vals <- vals[which(vals!='')]
  return(as.numeric(vals[2]))
}
