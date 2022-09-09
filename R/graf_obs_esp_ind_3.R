#' Gráfico dos valores observados e esperados para o qui-quadrado de independência, com barras vazias
#'
#' @param dados Banco de dados
#' @param v1 Variável categórica 1
#' @param v2 Variável categórica 2
#' @param xlab Título do eixo x (v1)
#' @param filllab Título do preechimento (v2)
#' @param cor Cores das barras
#' @param MacDonald Usar correção de MacDonald para os resíduos?
#'
#' @return
#' @export
#'
#' @examples
graf_obs_esp_ind_3 <- function(dados, v1, v2, xlab = "", filllab = "",
                               cor = c("grey75", "grey45"),
                               MacDonald = F){

  library(reshape2)

  v1s <- deparse(substitute(v1))
  v2s <- deparse(substitute(v2))

  if(xlab == ""){xlab <- v1s}else{}
  if(filllab == ""){filllab <- v2s}else{}

  n_lev <- length(levels(factor(dados[[v2s]])))

  tabela <- table(dados[[v1s]], dados[[v2s]])

  quiqua <- chisq.test(tabela)

  esp <- as.data.frame(melt(quiqua$expected))
  esp <- esp %>% rename(Esperado = value)
  obs <- as.data.frame(melt(quiqua$observed))
  obs <- obs %>% rename(Observado = value)
  banco_graf <- dplyr::left_join(esp, obs)
  banco_graf$max <- pmax(banco_graf$Esperado, banco_graf$Observado)
  banco_graf <- banco_graf %>%
    mutate(Var1 = factor(Var1),
           Var2 = factor(Var2))


  asteriscos <- as.data.frame(quiqua$stdres)

  if(isTRUE(MacDonald)){
    alfa <- 0.05/length(tabela)
    val_crit <- qnorm(p = alfa/2, lower.tail = FALSE)
  }else{val_crit <- 1.96}

  asteriscos <- asteriscos %>%
    mutate(sinal = case_when(between(Freq, val_crit*(-1), val_crit) ~ "",
                             TRUE ~ "*")) %>%
    mutate(Var1 = factor(Var1),
           Var2 = factor(Var2))

  banco_graf <- dplyr::left_join(banco_graf, asteriscos)
  banco_graf <- banco_graf %>% rename(!!v1s := Var1,
                                      !!v2s := Var2)


  if(length(cor)==1){
    if(n_lev == 2){
      cor <- c(cor, colorspace::lighten(cor, amount = 0.4))
    } else if(n_lev == 3){
      cor <- c(colorspace::darken(cor, amount = 0.4),
               cor, colorspace::lighten(cor, amount = 0.4))
    } else {
      paleta <- colorRampPalette(c(colorspace::lighten(cor, amount = 0.6),
                                   colorspace::darken(cor, amount = 0.6)))
      cor <- paleta(n_lev)
    }
  }else{}

  g <- banco_graf %>%
    ggplot(aes(x = {{v1}}, fill = {{v2}})) +
    geom_bar(aes(y = Observado), position = "dodge", stat = "identity") +
    geom_bar(aes(y = Esperado), position = "dodge", stat = "identity",
             color = "black", alpha = 0, show.legend = F) +
    geom_text(aes(y = max, label = sinal),
              position = position_dodge(width = 0.9), size = 4.5,
              vjust = 0.2) +
    labs(y = "Frequência (n)", x = xlab, fill = filllab) +
    scale_fill_manual(values = cor) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_classic() +
    theme(legend.position = "bottom")

  return(g)

}
