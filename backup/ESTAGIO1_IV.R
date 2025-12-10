# --- SCRIPT 4: ESTAGIO_1_IV.R (VERSÃO FINAL E CORRIGIDA) ---
# OBJETIVO: Replicar o Estágio 1 (First Stage - Tabela 7) e salvar as previsões.

library(fixest)
library(dplyr)
library(readr)
library(tidyr)
library(modelsummary) # Para etable

# --- 1. Carregar a Base de Dados Final (com dados do IV) ---
print("Carregando base de dados 'BASE_FINAL_TCC_PRAZO_IV.rds'...")
base_iv_completa <- readRDS("BASE_FINAL_TCC_PRAZO_IV.rds")

# --- 2. Preparar Dados para o Estágio 1 (Nível MUNICÍPIO-MÊS) ---
print("Agregando e filtrando dados para o nível MUNICÍPIO-MÊS (m,t)...")

# --- CORREÇÃO DO FILTRO TEMPORAL: Outubro (Pré-Pix) e Novembro (Pós-Pix) ---
# O artigo foca na mudança de Outubro para Novembro, o que define a janela de identificação.
base_filtrada <- base_iv_completa %>%
  filter(data == "202010" | data == "202011") 
# --- FIM DA CORREÇÃO ---

base_regressao_estagio1 <- base_filtrada %>%
  filter(!is.na(Eased_m)) %>% 
  
  # Agregação por MUNICÍPIO x MÊS. Se a base de Pix e Controles for única,
  # 'first()' garante que peguemos o valor correto para cada m-t.
  group_by(cod_mun, data) %>%
  summarise(
    log_pix_mt = first(log_pix_mt),
    Eased_m = first(Eased_m),
    Post_t = first(Post_t),
    Inst_1_Eased_x_Post = first(Inst_1_Eased_x_Post),
    
    # Controles Municipais (X_mt) - Não devem ser colineares com FEs de Município/Tempo
    # Mantemos aqui os controles demográficos, mas eles serão colineares com FEs MUNICÍPIO
    # e serão removidos se a FE de Município for incluída.
    log_populacao_m = first(log_populacao_m),
    log_pib_per_capita_m = first(log_pib_per_capita_m), 
    log_renda_per_capita_m = first(log_renda_per_capita_m), 
    share_populacao_rural = first(share_populacao_rural), 
    share_populacao_feminina = first(share_populacao_feminina), 
    share_populacao_jovem = first(share_populacao_jovem), 
    tx_analfabeto = first(tx_analfabeto),
    .groups = 'drop'
  ) %>%
  drop_na(log_pix_mt, Eased_m, Post_t, Inst_1_Eased_x_Post) %>% 
  
  mutate(
    cod_mun = as.factor(cod_mun),
    data = as.factor(data)
  )

print(paste("Obs. na base de dados ANTES do feols:", nrow(base_regressao_estagio1)))

# --- 3. Rodar a Regressão do Estágio 1 (Equação 8: OLS Município-Mês) ---
print("Rodando regressão do Estágio 1 (Equação 8)...")

controles_municipais <- c(
  "log_populacao_m", "log_pib_per_capita_m", 
  "log_renda_per_capita_m", "share_populacao_rural", 
  "share_populacao_feminina", "share_populacao_jovem", "tx_analfabeto"
)

# O modelo do artigo (Tabela 7) usa Efeitos Fixos de MUNICÍPIO (v_m) e de TEMPO (theta_t)
modelo_estagio_1 <- feols(
  as.formula(
    paste(
      "log_pix_mt ~ Eased_m + Post_t + Inst_1_Eased_x_Post + ",
      paste(controles_municipais, collapse = " + "),
      "| cod_mun + data" # FEs: Município (v_m) e Tempo (theta_t)
    )
  ),
  data = base_regressao_estagio1,
  cluster = "cod_mun"
)

# --- 4. Analisar e Salvar as Previsões ---
print("--- RESULTADO DO ESTÁGIO 1 (RÉPLICA DA TABELA 7) ---")

etable(modelo_estagio_1, keep = c("Eased_m", "Post_t", "Inst_1_Eased_x_Post"))

# ESTE É O PASSO CRUCIAL: Salvar as previsões de Pix (log_pix_mt_hat)
print("Salvando previsões (fitted values) alinhadas do Estágio 1...")

# 1. Gera previsões para todas as linhas da base usada
previsoes_com_na <- predict(modelo_estagio_1, newdata = base_regressao_estagio1)

# 2. Cria um data frame temporário com as chaves e as previsões (log_pix_mt_hat)
previsoes_full <- data.frame(
  cod_mun = base_regressao_estagio1$cod_mun,
  data = base_regressao_estagio1$data,
  log_pix_mt_hat = previsoes_com_na
)

# 3. Filtra apenas as linhas que TÊM uma previsão.
previsoes_estagio1 <- previsoes_full %>%
  filter(!is.na(log_pix_mt_hat))

# Verificação final
print(paste("Nº de linhas nas previsões salvas:", nrow(previsoes_estagio1)))
if (nrow(previsoes_estagio1) != modelo_estagio_1$nobs) {
  warning("ALERTA: O número de linhas das previsões não bate com as observações do modelo!")
} else {
  print("VERIFICADO: Alinhamento das previsões foi bem-sucedido.")
}

# Salva as previsões para usar no próximo script (Estágio 2)
saveRDS(previsoes_estagio1, "PREVISOES_ESTAGIO_1.rds")

print("--- SCRIPT 4 (ESTAGIO_1_IV.R) CONCLUÍDO ---")
print("Arquivo 'PREVISOES_ESTAGIO_1.rds' foi salvo.")