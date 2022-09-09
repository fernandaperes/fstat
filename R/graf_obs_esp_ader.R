#' Gráfico dos valores observados e esperados para o qui-quadrado de aderência
#'
#' @param dados Banco de dados
#' @param v1 Variável categórica
#' @param props Proporções esperadas das categorias
#' @param xlab Título eixo x
#' @param cor Cor das barras
#'
#' @return
#' @export
#'
#' @examples
graf_obs_esp_ader <- function(dados, v1, props = "", xlab = "", cor = c("grey75", "grey45")){

  library(reshape2)
  library(ggplot2)
  library(dplyr)

  v1s <- deparse(substitute(v1))

  tabela <- table(dados[[v1s]])

  if(xlab == ""){xlab <- v1s}else{}

  if(length(props)==1){
    p <- rep(1/length(tabela), length(tabela))
  }else{p <- props}

  quiqua <- chisq.test(tabela, p = p)

  esp <- as.data.frame(cbind(quiqua$expected))
  esp$Var <- row.names(esp)
  esp$v <- "Esperado"
  obs <- as.data.frame(cbind(quiqua$observed))
  obs$Var <- row.names(obs)
  obs$v <- "Observado"
  banco_graf <- rbind(esp, obs)
  colnames(banco_graf) <- c("Valor", deparse(substitute(v1)), "Resultado")


  asteriscos <- as.data.frame(quiqua$stdres)
  asteriscos <- asteriscos %>%
    mutate(sinal = case_when(between(Freq, -1.96, 1.96) ~ "",
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
    geom_text(aes(y = Valor+2, label = rotulos),
              position = position_dodge(width = 0.9), size = 4.5) +
    labs(y = "Frequência (n)", x = xlab, fill = NULL) +
    scale_fill_manual(values = cor) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_classic() +
    theme(legend.position = "bottom")

  return(g)

}
