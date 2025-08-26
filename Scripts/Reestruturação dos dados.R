# ==========================================================
# Minicurso R: Manipulação de Dados da Saúde Suplementar
# ==========================================================

# 1. Preparação do ambiente -------------------------------
rm(list = ls())
options(scipen = 9999)   # evita notação científica

# Carregar pacotes
library(readr)
library(tidyr)
library(dplyr)

# 2. Importação dos dados --------------------------------
message("Importando dados...")

procedimentos <- read_delim("Dados/Anual.txt", delim = "\t")
reajuste      <- read_delim("Dados/ReajusteAnual.txt", delim = "\t")
ipca_anual    <- read_delim("Dados/IPCA_anual.txt", delim = "\t")

# 3. Exploração inicial ----------------------------------
message("Explorando dados de procedimentos...")
names(procedimentos)
str(procedimentos)
unique(procedimentos$Porte)
unique(procedimentos$Ano)

# 4. Tratamento inicial ----------------------------------
message("Tratando variáveis numéricas...")
procedimentos <- procedimentos %>%
  mutate(Anual = gsub("\\.", "", Anual),   # remove pontos
         Anual = as.numeric(Anual))        # converte para número

# 5. Reestruturação e junção das bases -------------------
message("Reestruturando dados e unindo tabelas...")

# Totais por procedimento
procedimentos_agregados <- procedimentos %>%
  filter(Faixa %in% c("<1", "1 a 4", "5 a 9")) %>%
  group_by(Ano, Porte, Procedimento) %>%
  summarise(Total = sum(Anual, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Procedimento, values_from = Total)

# Totais por faixa etária
faixas <- procedimentos %>%
  group_by(Ano, Porte, Faixa) %>%
  summarise(Total = sum(Anual, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Faixa, values_from = Total)

# Totais gerais
totais <- procedimentos %>%
  group_by(Ano, Porte) %>%
  summarise(Quantidade = sum(Anual, na.rm = TRUE), .groups = "drop")

# Juntando todas as tabelas
tabela_final <- procedimentos_agregados %>%
  full_join(faixas, by = c("Ano", "Porte")) %>%
  full_join(totais, by = c("Ano", "Porte")) %>%
  full_join(reajuste, by = c("Ano", "Porte")) %>%
  left_join(ipca_anual, by = "Ano") %>%
  arrange(Ano, Porte)

# Substitui valores NA por 0
tabela_final[is.na(tabela_final)] <- 0

# 6. Criação de novas variáveis --------------------------
message("Criando variáveis derivadas...")

tabela_final <- tabela_final %>%
  mutate(
    menores_10       = `<1` + `1 a 4` + `5 a 9`,
    perc_menores_10  = (menores_10 / Quantidade) * 100,
    log_fisio        = log1p(Fisioterapia),
    log_fono         = log1p(Fonoaudiologia),
    log_psico        = log1p(Psicologia),
    log_to           = log1p(`Terapia Ocupacional`),
    log_quantidade   = log1p(Quantidade),
    log_bnf          = log1p(Beneficiários),
    log_reajuste     = log(Reajuste + 1)
  )

# 7. Exportação dos resultados ---------------------------
message("Salvando arquivos finais...")

write.csv(tabela_final, "Dados/tabela_reestruturada_anual.csv", row.names = FALSE)

# Também em Excel
library(writexl)
write_xlsx(tabela_final, "Dados/tabela_reestruturada_anual.xlsx")

message("Processo concluído com sucesso ✅")


#os dados foram consolidados por porte das OPSs (pequeno, médio e grande) em três bases

tabela_final_p <- subset(tabela_final, Porte == "Pequeno")
tabela_final_m   <- subset(tabela_final, Porte == "Médio")
tabela_final_g  <- subset(tabela_final, Porte == "Grande")

write_xlsx(tabela_final, "Dados/tabela_final_p.xlsx")
write_xlsx(tabela_final, "Dados/tabela_final_m.xlsx")
write_xlsx(tabela_final, "Dados/tabela_final_g.xlsx")
