#' Cria um gráfico de floresta para a regressão linear
#'
#' @param mod Modelo de regressão linear (criado pela função lm)
#'
#' @return
#' @export
#'
#' @examples
forest_linear <- function(mod){

  library(dplyr)
  library(ggplot2)
  library(broom)
  library(broom.helpers)
  library(ggplot2)

  tabela <- mod %>%
    broom.helpers::tidy_and_attach(conf.int = TRUE, exponentiate = F) %>%
    broom.helpers::tidy_add_estimate_to_reference_rows() %>%
    mutate(estimate = ifelse((reference_row == T & (estimate == 1 | estimate == 0)), NA, estimate)) %>%
    mutate(levels = ifelse((var_class == "factor" | var_class == "character"), gsub("[(`)]", "", stringr::str_remove(term, variable)), "")) %>%
    select(variable, levels, estimate, conf.low, conf.high, p.value, reference_row, var_class)

  refs <- tabela %>% filter(variable != "(Intercept)") %>%
    filter(reference_row == T) %>%
    mutate(ref = paste0("\n(Referência = ", levels, ")")) %>%
    select(variable, ref)

  tabela_graf <- dplyr::left_join(tabela, refs, by = "variable") %>%
    mutate(ref = ifelse(is.na(ref), "", ref)) %>%
    mutate(variable = ifelse(levels != "", paste0(variable, " - ", levels, ref),
                             variable)) %>%
    filter(reference_row == FALSE | is.na(reference_row) & variable != "(Intercept)") %>%
    rename(`Variável independente` = variable)

  graf <- tabela_graf %>%
    ggplot(aes(y = `Variável independente`, x = estimate)) +
    geom_vline(aes(xintercept = 0), linewidth = 1, color = "grey70") +
    geom_point() +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
    scale_x_continuous(labels = scales::number_format(decimal.mark = ",")) +
    labs(y = NULL, x = "Coeficiente (B) não-padronizado") +
    theme_bw()

  return(graf)

}
