#' @title Get Coporation's Perfomance
#' @description description
#' @usage getPerf('035420')
#' @param code stockCode
#' @return value
#' @import rvest
#' @import dplyr
#' @import xml2
#'
#' @export

getPerf <- function(code = NULL, recent = TRUE){
  if(is.null(code)) stop("Code not given")
  url_main <- paste0('https://finance.naver.com/item/main.nhn?code=',code)
  mainPage <- read_html(url_main, encoding = 'CP949')
  vals <- mainPage %>%
    html_node('.section.cop_analysis .sub_section') %>%
    html_text() %>%
    gsub(pattern = '\t',replacement = '') %>%
    gsub(pattern = ',',replacement = '') %>%
    gsub(pattern = '\u00A0', replacement = ' ') %>%
    strsplit(split = '\n') %>%
    unlist()
  vals <- vals[-1]
  vals <- vals[which(vals!='')]
  vals <- vals[which(vals!='IFRS연결')]

  vals <- vals[-c(1:4)]
  nm <- vals[1:10]
  vals <- vals[-c(1:19)] # name + empty header cell

  # perf
  res <- matrix(vals[1:143], ncol = 11, byrow = TRUE)
  rownames(res) <- res[,1]
  res <- res[,-1]
  colnames(res)<- nm

  # share
  c1 <- '주당배당금(원)'
  c2 <- '시가배당률(%)'
  c3 <- '배당성향(%)'
  idx1 <- which(vals==c1)
  idx2 <- which(vals==c2)
  idx3 <- which(vals==c3)

  v1 <- vals[(idx1+1):(idx2-1)]
  while(length(v1)%%10!=0){
    v1 <- c(v1, ' ')
  }

  v2 <- vals[(idx2+1):(idx3-1)]
  while(length(v2)%%10!=0){
    v2 <- c(v2, ' ')
  }
  v3 <- vals[(idx3+1):length(vals)]
  while(length(v3)%%10!=0){
    v3 <- c(v3, ' ')
  }

  res <- rbind(res, v1, v2, v3)
  rownames(res)[14:16] = c(c1,c2,c3)
  if(recent){
    return(res[,-c(1:2, 5:7)])
  }
  return(res)
}


