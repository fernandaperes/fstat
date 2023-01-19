#' Formata o valor de p
#'
#' @param x Valor de p a ser formatado
#' @param idioma Caso "PT", a vírgula passa a ser o separador de decimal
#'
#' @return Valor de p formatado
#' @export
#'
#' @examples
#' p_val(0.00876)
#' p_val(0.0004)
#'
p_val <- function(x) {

  ifelse(x < 0.001, "< 0,001",
         scales::number(x, accuracy = 0.001, decimal.mark = ","))

  return(x)

}
