#' @title Generate / Send daily Report
#' @description creds_key with id = 'gmail' should be declared before
#' @usage mailMe()
#' @return NO RETURN
#' @import blastula
#' @import knitr
#' @seealso blastula::create_smtp_creds_key
#' @export

mailMe <- function(){
  email <- render_email('report.rmd')
  email %>% smtp_send(
      from = 'hwanistic@gmail.com',
      to = 'hwanistic@gmail.com',
      subject = 'testing smtp_send function',
      credentials = creds_key(id = 'gmail')
    )
}

