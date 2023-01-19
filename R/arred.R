#' Formata números para impressão
#'
#' @param x Número a ser formatado
#' @param dm Separador de decimal
#' @param digitos Quantidade de dígitos
#' @param bm Separador de milhar
#'
#' @return Número formatado
#' @export
#'
#' @examples
#' arred(25.8549, digitos = 2)
#'
arred <- function(x, digitos = 3, dm = ",", bm = "") {
  acc <- 10^-(digitos)
  val <- scales::number(x, accuracy = acc, decimal.mark = dm,
                        big.mark = bm)
  return(val)
}
