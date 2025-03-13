#' Criação do dicionário de variáveis
#'
#' @param dados
#'
#' @return
#' @export
#'
#' @examples
dicionario <- function(dados){

  dicionario <- data.frame(coluna = 1:(dim(dados)[2]),
                           variavel = names(dados),
                           tipo = unlist(sapply(lapply(dados, class), "[[", 1)))

  ntotal <- dim(dados)[2]
  n_porcol <- apply(is.na(dados) == F, 2, sum)

  dicionario <- data.frame(dicionario)
  dicionario$niveis <- lapply(dados, levels)

  for(i in 1:ntotal){dicionario$n_niveis[i] <- length(dicionario$niveis[[i]])}

  dicionario$n <- n_porcol
  dicionario$porc_aus <- 100*(dim(dados)[1] - dicionario$n)/dim(dados)[1]

  row.names(dicionario) <- 1:length(dicionario$pos)

  return(dicionario)

}
