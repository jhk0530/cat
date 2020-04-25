#' @title Get daily price information
#' @description description
#' @usage getPrice('035420')
#' @param code stockCode
#' @return vector with price, volume, low, high, changed, changed ratio
#' @import rvest
#' @import xml2
#' @export

getPrice <- function(code = NULL){

  if(is.null(code)) stop("Code not given")

  # naver uses CP949 encoding
  url_sise <- paste0('https://finance.naver.com/item/sise.nhn?code=', code)
  sisePage <- read_html(url_sise, encoding = 'CP949')

  ValueWithID <- function(Page, id = NULL){
    id <- paste0('#', id)
    Page %>%
      html_nodes(id) %>%
      html_text() %>%
      gsub(pattern = ',',replacement = '') %>%
      as.numeric()
  }

  ValueIncDesc <- function(Page, id = '_diff'){
    id <- paste0('#', id)
    Val <- Page %>%
      html_nodes(id) %>%
      html_text() %>%
      gsub(pattern = '\t', replacement = '') %>%
      strsplit(split = '\n') %>%
      unlist() %>%
      unique() %>%
      sort() %>%
      gsub(pattern = '%', replacement = '') %>%
      gsub(pattern = ',', replacement = '')

    return(as.numeric(Val[2]))
  }

  res <-
    c(
      sisePage %>% ValueWithID('_nowVal'), # 종가
      sisePage %>% ValueWithID('_quant'), # 거래량
      sisePage %>% ValueWithID('_low'), # 저가
      sisePage %>% ValueWithID('_high'), # 고가
      sisePage %>% ValueIncDesc(), # 등락가
      sisePage %>% ValueIncDesc(id ='_rate') # 등락률
    )
  names(res) = c("종가", "거래량", "저가", "고가", "등락가", "등락률")
  res
}
