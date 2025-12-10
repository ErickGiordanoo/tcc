# --- SCRIPT 5: MODELO_PRAZO_IV.R (ESTÁGIO 2) ---
# OBJETIVO: Estimar o modelo causal (Estágio 2) usando as 
#           previsões salvas do Estágio 1.

# --- 1. Carregar Pacotes ---
library(fixest)
library(dplyr)
library(readr)
library(tidyr) 

# --- 2. Carregar as Duas Bases de Dados ---
print("Carregando base de dados 'BASE_FINAL_TCC_PRAZO_IV.rds'...")
base_painel_final <- readRDS("BASE_FINAL_TCC_PRAZO_IV.rds") # (Base completa 2020-2025)

print("Carregando previsões do Estágio 1 ('PREVISOES_ESTAGIO_1.rds')...")
previsoes_estagio1 <- readRDS("PREVISOES_ESTAGIO_1.rds") # (Base filtrada ~7k obs)

# --- 3. Preparar Dados para o Estágio 2 (Nível Banco-Município-Mês) ---
print("Preparando dados para o Estágio 2...")

base_regressao_estagio2 <- base_painel_final %>%
  # 1. Juntar (merge) as previsões do Estágio 1 na base principal
  # O 'inner_join' AQUI FUNCIONA COMO O FILTRO TEMPORAL
  # Ele irá manter apenas as obs. de banco-mun-mês de Out/Nov 2020
  inner_join(previsoes_estagio1, by = c("cod_mun", "data")) %>%
  
  # 2. Criar a variável de interação instrumentada
  mutate(
    # Esta é a sua variável instrumentada (prevista)
    fit_log_pix_mt = log_pix_mt_hat, 
    
    # Esta é a sua INTERAÇÃO instrumentada (prevista)
    fit_pix_x_Si = log_pix_mt_hat * Si 
  ) %>%
  
  # 3. Remover NAs restantes (especialmente da variável dependente b_it_prazo)
  drop_na(
    b_it_prazo, fit_log_pix_mt, fit_pix_x_Si, Si, Eased_m,
    hhi_m_fixo, log_ativos_it, log_populacao_m, log_pib_per_capita_m, 
    log_renda_per_capita_m, share_populacao_rural, 
    share_populacao_feminina, share_populacao_jovem, tx_analfabeto,
    cnpj, data, cod_mun
  ) %>%
  
  # 4. Converter para fatores
  mutate(
    cnpj = as.factor(cnpj),
    data = as.factor(data),
    cod_mun = as.factor(cod_mun)
  )

print(paste("Obs. para o Estágio 2 (Banco-Município-Mês):", nrow(base_regressao_estagio2)))
print("---------------------------------")

# --- 4. Definir a Fórmula de Regressão do Estágio 2 (OLS/FE) ---
print("Definindo a fórmula do Estágio 2...")

controles_estagio2 <- c(
  "Si", "Eased_m", # Incluímos Si e Eased_m como controles
  "hhi_m_fixo", "log_ativos_it", "log_populacao_m", 
  "log_pib_per_capita_m", "log_renda_per_capita_m", 
  "share_populacao_rural", "share_populacao_feminina", 
  "share_populacao_jovem", "tx_analfabeto"
)

formula_estagio_2 <- as.formula(
  paste(
    # Y ~ X_instrumentado_1 + X_instrumentado_2 + Controles
    "b_it_prazo ~ fit_log_pix_mt + fit_pix_x_Si + ",
    paste(controles_estagio2, collapse = " + "),
    
    # Efeitos Fixos de Banco (cnpj) e Mês (data)
    "| cnpj + data"
  )
)

print("Fórmula do Modelo Estágio 2 (OLS com variáveis previstas):")
print(formula_estagio_2)

# --- 5. Rodar o Modelo Estágio 2 ---
print("Rodando o Modelo do Estágio 2...")

modelo_estagio_2_final <- feols(
  fml = formula_estagio_2,
  data = base_regressao_estagio2,
  cluster = "cod_mun" 
)

# --- 6. Exibir a Tabela de Regressão Final ---
print("--- RESULTADO: ESTÁGIO 2 (Second Stage) - MODELO CAUSAL ---")

dict_vars_iv <- c(
  "b_it_prazo" = "Dep.Var: (b_it)",
  "fit_log_pix_mt" = "Pix (Instrumentado)", 
  "fit_pix_x_Si" = "Pix * Small (Instrumentado)",
  "Si" = "Banco Pequeno (Dummy)",
  "Eased_m" = "Relaxou COVID (Dummy)", 
  "hhi_m_fixo" = "HHI Pré-Pix"
)

keep_vars_final <- c(
  "Pix (Instrumentado)", 
  "Pix * Small (Instrumentado)",
  "Banco Pequeno (Dummy)",
  "Relaxou COVID (Dummy)",
  "HHI Pré-Pix"
)

etable(
  modelo_estagio_2_final,
  
  dict = dict_vars_iv,
  title = "Impacto Causal do Pix no Poder de Mercado (Modelo IV-2SLS Manual)",
  
  keep = keep_vars_final, 
  
  style.tex = style.tex("aer"), 
  signif.code = c("***"=0.01, "**"=0.05, "*"=0.10),
  fitstat = c("n") # Mostra o número de observações
)

print("--- SCRIPT 5 (MODELO_PRAZO_IV.R) CONCLUÍDO ---")