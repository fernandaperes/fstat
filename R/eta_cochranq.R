#' Maximum-corrected measure of effect size eta - Para o teste Q de Cochran
#'
#' @param teste O resultado do teste Q de Cochran feito pela função rstatix::cochran_qtest
#'
#' @return
#' @export
#'
#' @examples
eta_cochranq <- function(teste){

  ## Maximum-corrected measure of effect size eta - Serlin, Carr and Marascuillo's (2007)

  q <- teste$statistic

  b <- teste$n

  df <- teste$df
  # df corresponde a k-1

  eta <- q/(b*df)

  return(eta)

}
