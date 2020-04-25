#' @title Get Foreign / Organization trend information
#' @description this will not include last day's information.
#' @usage getFrgn('035420')
#' @param code stockCode
#' @return data.frame with
#' @import rvest
#' @import dplyr
#' @import xml2
#'
#' @export

getFrgn <- function(code = NULL){
  if(is.null(code)) stop("Code not given")
  url_frgn <- paste0('https://finance.naver.com/item/frgn.nhn?code=',code)
  frgnPage <- read_html(url_frgn, encoding = 'CP949')
  vals <- frgnPage %>%
    html_node('#content > div.section.inner_sub > table.type2') %>%
    html_text() %>%
    gsub(pattern = '\t', replacement = '') %>%
    gsub(pattern = ',', replacement = '') %>%
    gsub(pattern = '%',replacement = '') %>%
    strsplit(split = '\n') %>%
    unlist()
  vals <- vals[which(vals != '')]


  vals <- vals[-c(1:12)] # remove names

  res <- data.frame(matrix(vals, ncol =9, byrow = TRUE))
  rownames(res) <- res[,1]
  res <- res[,-1]
  colnames(res) <- c("종가", "전일비", "등락률", "거래량", "기관 매매량", "외국인 매매량", "보유주수", "보유율")
  res <- res %>% select(-"보유주수", -"보유율")
  res
}
