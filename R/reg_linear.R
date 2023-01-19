#' Tabela e gráfico para regressão linear
#'
#' @param mod
#' @param add_global_p
#'
#' @return
#' @export
#'
#' @examples
reg_linear <- function(mod, add_global_p = F){

  library(tibble)
  library(broom)
  library(broom.helpers)
  library(dplyr)
  library(ggplot2)

  tabela <- mod %>%
    broom.helpers::tidy_and_attach(conf.int = TRUE, exponentiate = F) %>%
    broom.helpers::tidy_add_estimate_to_reference_rows() %>%
    mutate(estimate = ifelse((reference_row == T & (estimate == 1 | estimate == 0)), NA, estimate)) %>%
    mutate(levels = ifelse((var_class == "factor" | var_class == "character"), gsub("[(`)]", "", stringr::str_remove(term, variable)), "")) %>%
    select(variable, levels, estimate, conf.low, conf.high, p.value, reference_row, var_class)

  n_cat <- tabela %>% filter(var_class == "factor" | var_class == "character") %>% count()
  n_num <- tabela %>% filter(var_class == "numeric") %>% count()

  tabela_res_cat <- tabela %>%
    filter(var_class == "factor" | var_class == "character") %>%
    group_by(variable) %>%
    group_modify(~ add_row(.x, .before = 0))

  tabela_res_cont <- tabela %>%
    filter(var_class == "numeric")

  if(n_cat > 0 && n_num > 0){tabela_res <- rbind(tabela_res_cat, tabela_res_cont)
  }else{if(n_cat == 0){tabela_res <- tabela_res_cont
  }else{tabela_res <- tabela_res_cat}}

  tabela_res <- tabela_res %>%
    mutate(B = arred(estimate, 3),
           `IC 95%` = ifelse(is.na(conf.low), NA, paste0(arred(conf.low, 3),
                                                         "; ",
                                                         arred(conf.high, 3))),
           p = p_val(p.value),
           levels = ifelse(levels == "", NA, levels)) %>%
    mutate_at(c(9:10), ~ifelse(!is.na(levels), replace(., is.na(.), "---"), .)) %>%
    mutate(variable = ifelse(!is.na(levels), "    ", variable)) %>%
    mutate(levels = ifelse(is.na(levels), "", levels)) %>%
    mutate(variable = paste0(variable, levels)) %>%
    rename(`Variável independente` = variable) %>%
    select(`Variável independente`, B, `IC 95%`, p)

  global_p <- as.data.frame(car::Anova(mod))
  global_p <- rownames_to_column(global_p, var = "variable")
  global_p <- global_p %>% select(1, 4)
  colnames(global_p) <- c("Variável independente", "pval")
  global_p <- global_p %>% mutate(pval = p_val(pval),
                                  `Variável independente` = gsub("`", "", `Variável independente`))

  if(isTRUE(add_global_p)){
    tabela_res <- dplyr::left_join(tabela_res, global_p) %>%
      mutate(p = ifelse(is.na(p), pval, p)) %>%
      select(-5)
  }else{}

  refs <- tabela %>% filter(variable != "(Intercept)") %>%
    filter(reference_row == T) %>%
    mutate(ref = paste0("\n(Categoria de referência = ", levels, ")")) %>%
    select(variable, ref)

  tabela_graf <- dplyr::left_join(tabela, refs, by = "variable") %>%
    mutate(ref = ifelse(is.na(ref), "", ref)) %>%
    mutate(variable = ifelse(levels != "", paste0(variable, " - ", levels, ref),
                             variable)) %>%
    filter(reference_row == FALSE | is.na(reference_row) & variable != "(Intercept)") %>%
    mutate(variable = ifelse(p.value < 0.05, paste0(variable, "*"), variable)) %>%
    rename(`Variável independente` = variable)

  graf <- tabela_graf %>%
    ggplot(aes(y = `Variável independente`, x = estimate)) +
    geom_vline(aes(xintercept = 0), size = 1, color = "grey70") +
    geom_point() +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high)) +
    scale_x_continuous(labels = scales::number_format(decimal.mark = ",")) +
    labs(y = NULL, x = "Coeficiente (B) não-padronizado") +
    theme_bw()

  return(list(tabela = tabela_res,
              graf = graf))

}
