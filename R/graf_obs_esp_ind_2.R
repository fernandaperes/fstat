#' Gráfico dos valores observados e esperados para o qui-quadrado de independência, mostrando todas as categorias
#'
#' @param dados Banco de dados
#' @param v1 Variável categórica 1
#' @param v2 Variável categórica 2
#' @param xlab Título do eixo x
#' @param cor Cor das barras
#' @param MacDonald Usar correção de MacDonald para os resíduos?
#' @param asterisco Incluir asteriscos no gráfico?
#'
#' @return
#' @export
#'
#' @examples
graf_obs_esp_ind_2 <- function(dados, v1, v2, xlab = "",
                               cor = c("grey75", "grey45"),
                               asterisco = T,
                               MacDonald = F){

  library(reshape2)
  library(ggplot2)
  library(dplyr)

  v1s <- deparse(substitute(v1))
  v2s <- deparse(substitute(v2))

  if(xlab == ""){xlab <- paste(v1s, "/ ", v2s)}else{}

  tabela <- table(dados[[v1s]], dados[[v2s]])

  quiqua <- chisq.test(tabela)

  esp <- as.data.frame(melt(cbind(quiqua$expected)))
  esp$v <- "Esperado"
  obs <- as.data.frame(melt(cbind(quiqua$observed)))
  obs$v <- "Observado"
  banco_graf <- rbind(esp, obs)
  colnames(banco_graf) <- c(deparse(substitute(v1)), deparse(substitute(v2)),
                            "Valor", "Resultado")

  asteriscos <- as.data.frame(quiqua$stdres)

  if(isTRUE(MacDonald)){
    alfa <- 0.05/length(tabela)
    val_crit <- qnorm(p = alfa/2, lower.tail = FALSE)
  }else{val_crit <- 1.96}

  asteriscos <- asteriscos %>%
    mutate(sinal = case_when(between(Freq, val_crit*(-1), val_crit) ~ "",
                             TRUE ~ "*"))

  asteriscos <- asteriscos$sinal
  vazios <- rep("", length(asteriscos))
  rotulos <- c(vazios, asteriscos)

  if(length(cor)==1){
    cor <- c(cor, colorspace::lighten(cor, amount = 0.4))
  }else{}


  g <- banco_graf %>%
    ggplot(aes(x = {{v1}}, y = Valor, fill = Resultado)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(y = "Frequência (n)", x = xlab, fill = NULL, vjust = -1) +
    scale_fill_manual(values = cor) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_classic() +
    facet_wrap(enquo(v2), strip.position = "bottom") +
    theme(legend.position = "bottom",
          strip.placement = "outside",
          strip.background = element_rect(fill = NA, color = NA))

  if(isTRUE(asterisco)){g <- g + geom_text(aes(y = Valor, label = rotulos),
                                            position = position_dodge(width = 0.9), size = 4.5)}else{}

  return(g)

}
