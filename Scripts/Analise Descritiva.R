# -----------------------------------------------------------
# ANÁLISE DESCRITIVA DAS VARIÁVEIS (por porte)
# Resultado: uma tabela para cada variável
# -----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(gridExtra)

# 1. Selecionar as variáveis de interesse + porte
dados_desc <- tabela_final %>%
  select(Porte, Ano, log_reajuste, log_bnf, perc_menores_10, 
         log_fisio, log_fono, log_psico, log_to, IPCA_A)

# 2. Função para gerar estatísticas descritivas de uma variável por porte
estat_desc <- function(data, var){
  data %>%
    group_by(Porte) %>%
    summarise(
      Media   = mean(.data[[var]], na.rm = TRUE),
      Mediana = median(.data[[var]], na.rm = TRUE),
      Min     = min(.data[[var]], na.rm = TRUE),
      Max     = max(.data[[var]], na.rm = TRUE),
      DP      = sd(.data[[var]], na.rm = TRUE),
      CV      = (sd(.data[[var]], na.rm = TRUE) / mean(.data[[var]], na.rm = TRUE)) * 100,
      .groups = "drop"
    ) %>%
    mutate(Variavel = var, .before = 1)
}

# 3. Gerar lista de tabelas para cada variável
variaveis <- names(dados_desc)[-c(1,2)] # remove Porte e Ano
tabelas_resultado <- lapply(variaveis, function(v) estat_desc(dados_desc, v))
names(tabelas_resultado) <- variaveis

# 4. Exibir tabelas separadas
for(v in variaveis){
  cat("\n========================================\n")
  cat("Estatísticas descritivas -", v, "\n")
  cat("========================================\n")
  print(tabelas_resultado[[v]])
}

# 5. Converter Ano em fator
dados_desc$Ano <- as.factor(dados_desc$Ano)

# 6. Gráficos de linhas por porte
graficos_linhas <- list()
for (var in variaveis) {
  p <- ggplot(dados_desc, aes(x = Ano, y = .data[[var]], color = Porte, group = Porte)) +
    geom_line(size = 1) +
    geom_point(size = 1.5, alpha = 0.8) +
    labs(title = paste("Evolução de", var, "por Porte"),
         x = "Ano", y = var) +
    theme_minimal(base_size = 14)
  
  graficos_linhas[[var]] <- p
}

# Exibir gráficos em grid
do.call(grid.arrange, c(graficos_linhas, ncol = 2))
