#' Chance corrected measure of effect size r - Para o teste Q de Cochran
#'
#' @param df Banco de dados no formato wide e excluindo o ID
#'
#' @return
#' @export
#'
#' @examples
r_cochranq <- function(df){

  ## Chance corrected measure of effect size - Berry, Johnston and Mielke (2007)
  ## Baseado em: https://stats.stackexchange.com/questions/9867/effect-size-of-cochrans-q
  ## Implementado em R por: https://youtu.be/JGx2YfrceYk

  b <- dim(df)[1]
  k <- dim(df)[2]
  df <- df %>% mutate(mutate(across(everything(), ~ as.numeric(.)-1)))
  matriz <- as.matrix(df)
  res <- 0
  for (i in 1:k){
    for (j in 1:(b-1)){
      for (l in (j+1):b){
        res <- res + abs(matriz[j,i] - matriz[l,i])
      }
    }
  }
  delta <- 1/(k*choose(b,2))*res
  pi <- rowSums(matriz)/k
  spi <- 0
  spi2 <- 0
  for (i in 1:b){
    spi<-spi+pi[i]
    spi2<-spi2+pi[i]*(1-pi[i])
  }
  mudelta<-2/(b*(b-1))*(spi*(b-spi)-spi2)
  r <- unname(1-delta/mudelta)

  return(r)

}
