# ------------------------------------------------------------
# Modelagem dos Reajustes por Porte da Operadora
# ------------------------------------------------------------

# Pacotes necessários
library(lmtest)     # Testes estatísticos (Breusch-Pagan, Durbin-Watson etc.)
library(sandwich)   # Erros-padrão robustos (Newey-West)

# ------------------------------------------------------------
# 1. Estimação dos modelos iniciais (sem seleção stepwise)
# ------------------------------------------------------------
# Obs: aqui usamos as variáveis explicativas originais.
# O resultado inicial serve como ponto de partida.

modelo_p <- lm(log_reajuste ~ log_bnf + perc_menores_10 + IPCA_A + 
                 log_fisio + log_fono + log_psico + log_to, 
               data = tabela_final_p)

modelo_m <- lm(log_reajuste ~ log_bnf + perc_menores_10 + IPCA_A + 
                 log_fisio + log_fono + log_psico + log_to, 
               data = tabela_final_m)

modelo_g <- lm(log_reajuste ~ log_bnf + perc_menores_10 + IPCA_A + 
                 log_fisio + log_fono + log_psico + log_to, 
               data = tabela_final_g)

# Visualizar os resultados dos modelos iniciais
summary(modelo_p)
summary(modelo_m)
summary(modelo_g)

# ------------------------------------------------------------
# 2. Seleção Stepwise (critério AIC)
# ------------------------------------------------------------
# Isso ajuda a simplificar os modelos, retirando variáveis sem significância.

modelo_final_p <- step(modelo_p, direction = "both")
modelo_final_m <- step(modelo_m, direction = "both")
modelo_final_g <- step(modelo_g, direction = "both")

# Resultados dos modelos finais após o stepwise
summary(modelo_final_p)
summary(modelo_final_m)
summary(modelo_final_g)

# ------------------------------------------------------------
# 3. Diagnóstico dos Modelos
# ------------------------------------------------------------
# 3.1 Teste de Normalidade dos Resíduos (Shapiro-Wilk)
shapiro.test(residuals(modelo_final_p))  # porte pequeno
shapiro.test(residuals(modelo_final_m))  # porte médio
shapiro.test(residuals(modelo_final_g))  # porte grande

# 3.2 Teste de Homocedasticidade (Breusch-Pagan)
bptest(modelo_final_p)
bptest(modelo_final_m)
bptest(modelo_final_g)

# 3.3 Teste de Autocorrelação (Durbin-Watson)
dwtest(modelo_final_p)
dwtest(modelo_final_m)
dwtest(modelo_final_g)

# ------------------------------------------------------------
# 4. Correção para Autocorrelação/Heterocedasticidade
# ------------------------------------------------------------
# Utilizamos a matriz de variância-covariância robusta de Newey-West.
# Isso corrige os erros-padrão SEM alterar os coeficientes do modelo.

coeftest(modelo_final_p, vcov = NeweyWest(modelo_final_p))
#coeftest(modelo_final_m, vcov = NeweyWest(modelo_final_m))
#coeftest(modelo_final_g, vcov = NeweyWest(modelo_final_g))

# ------------------------------------------------------------
# Observações finais:
# - Sempre interpretar os coeficientes junto com os testes de diagnóstico.
# - Verifique se os resíduos são aproximadamente normais e homocedásticos.
# - Caso contrário, usar modelos alternativos (GLS, modelos em painel etc.).
# ------------------------------------------------------------
