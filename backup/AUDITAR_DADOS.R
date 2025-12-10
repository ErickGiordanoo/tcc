# --- SCRIPT: AUDITAR_DADOS.R (VERSÃO PÓS-CORREÇÃO - FOCO NO MODELO PRAZO E HORIZONTE DE TEMPO) ---
# OBJETIVO: Auditar a base de dados final ('BASE_FINAL_TCC_PRAZO.rds')
#           NO HORIZONTE DE TEMPO EXATO UTILIZADO NO MODELO.

library(dplyr)
library(ggplot2)

# --- 1. CONFIGURAÇÃO DE HORIZONTE DE TEMPO ---
# Ajuste estas datas para corresponder ao seu MODELO_PRAZO.R
data_inicio <- "202011" 
data_fim <- "202311"    

# --- 2. Carregar a Base de Dados Final (Corrigida) ---
print("Carregando BASE_FINAL_TCC_PRAZO.rds...")
tryCatch({
  base_final_raw <- readRDS("BASE_FINAL_TCC_PRAZO.rds") 
  print(paste("Base carregada com sucesso.", nrow(base_final_raw), "linhas (i-m-t)."))
}, error = function(e) {
  print("ERRO: Não foi possível carregar 'BASE_FINAL_TCC_PRAZO.rds'.")
  stop(e)
})

# --- 3. APLICAR FILTRO DE TEMPO E TRATAMENTOS FINAIS ---
print(paste("Filtrando dados para o horizonte de tempo:", data_inicio, "a", data_fim, "..."))

base_final <- base_final_raw %>%
  # APLICAÇÃO DO FILTRO DE TEMPO
  filter(data >= data_inicio & data <= data_fim) %>%
  
  # Replicando tratamentos finais (Log) do script MERGE.R para garantir logs corretos.
  mutate(
    log_pib_per_capita_m = log(pmax(pib_per_capita_m, 0) + 1),
    log_renda_per_capita_m = log(pmax(renda_per_capita_m, 0) + 1)
  )

print(paste("Linhas após o filtro de tempo:", nrow(base_final)))


# --- 4. Função Auxiliar de Auditoria ---
audit_variable <- function(df, var_name, var_level) {
  if (!var_name %in% names(df)) {
    print("------------------------------------------------------")
    print(paste("ERRO DE AUDITORIA: A coluna", var_name, "NÃO FOI ENCONTRADA no dataframe."))
    print("------------------------------------------------------")
    return(NULL)
  }
  var_vector <- df[[var_name]]
  
  print("------------------------------------------------------")
  print(paste("AUDITANDO:", var_name, "(Nível:", var_level, ")"))
  
  # 1. Checar NAs
  na_count <- sum(is.na(var_vector))
  if (na_count > 0) {
    na_percent <- round(na_count / length(var_vector) * 100, 2)
    print(paste("ALERTA: Encontrados", na_count, "valores NA (", na_percent, "% )."))
  } else {
    print("SUCESSO: Nenhum valor NA encontrado.")
  }
  
  # 2. Sumário Estatístico
  print("Sumário Estatístico:")
  print(summary(var_vector, na.rm = TRUE))
  
  # 3. Teste de Outlier (IQR)
  if (is.numeric(var_vector) && !all(is.na(var_vector)) && var(var_vector, na.rm = TRUE) > 0) {
    q1 <- quantile(var_vector, 0.25, na.rm = TRUE)
    q3 <- quantile(var_vector, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    limite_superior <- q3 + (1.5 * iqr)
    limite_inferior <- q1 - (1.5 * iqr)
    outliers_count <- sum(var_vector > limite_superior | var_vector < limite_inferior, na.rm = TRUE)
    
    print(paste("Limite Superior (Outlier):", round(limite_superior, 2)))
    print(paste("Limite Inferior (Outlier):", round(limite_inferior, 2)))
    print(paste("Total de Outliers (IQR):", outliers_count))
  } else {
    print("Não é possível calcular limites de outlier (Não-numérico ou Variância zero).")
  }
  cat("\n")
}

# --- 5. Criar Bases Únicas por Nível (APÓS O FILTRO DE TEMPO) ---
print("Criando bases únicas para auditoria no nível correto (dentro do horizonte)...")

# As bases únicas agora refletem apenas os dados dentro do período 202001-202507
df_it <- base_final %>% distinct(cnpj, data, .keep_all = TRUE)
df_m <- base_final %>% distinct(cod_mun, .keep_all = TRUE)
df_i <- base_final %>% distinct(cnpj, .keep_all = TRUE)

# --- 6. Executar Auditoria (Console) ---
print("--- INICIANDO AUDITORIA NO CONSOLE ---")

# 6a. Variável Dependente (Nível i-t)
audit_variable(df_it, "b_it_prazo", "i-t (Banco-Mês)")

# 6b. Variáveis Independentes Principais (Tratadas)
audit_variable(df_m, "hhi_m_fixo", "m (Município-Mês)") 
audit_variable(base_final, "log_pix_mt", "i-m-t (Banco-Mun-Mês)")

# 6c. Controles (Tratados)
audit_variable(base_final, "log_ativos_it", "i-m-t (Banco-Município-Mês)")
audit_variable(df_m, "log_populacao_m", "m (Município)")
audit_variable(df_m, "log_pib_per_capita_m", "m (Município)")
audit_variable(df_m, "log_renda_per_capita_m", "m (Município)")
audit_variable(df_m, "share_populacao_rural", "m (Município)")
audit_variable(df_m, "share_populacao_feminina", "m (Município)")
audit_variable(df_m, "tx_analfabeto", "m (Município)")

# 6d. Termo de Interação (Tratado)
audit_variable(base_final, "pix_x_Si", "i-m-t (Interação)") 

print("--- AUDITORIA DO CONSOLE CONCLUÍDA ---")


# --- 7. Visualização (Gráfico Painel) - VARIÁVEIS DO MODELO ---
print("--- 7. Gerando Painel de Histogramas (AUDITORIA_PRAZO_HISTOGRAMAS.png) ---")

# Nível i-t (Betas, Variável Dependente)
df_it_long <- df_it %>%
  select(b_it_prazo) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")

# Nível m (HHI e Controles de Município)
df_m_long <- df_m %>%
  select(
    hhi_m_fixo, 
    log_populacao_m,  
    log_pib_per_capita_m,
    log_renda_per_capita_m,
    share_populacao_rural, 
    share_populacao_feminina, 
    tx_analfabeto
  ) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")


# Nível i-m-t (Controles e Interação)
df_b_long <- base_final %>%
  sample_n(min(nrow(base_final), 50000)) %>% 
  select(
    log_ativos_it, 
    log_pix_mt, 
    pix_x_Si    
  ) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor")

# Combinar todos os dataframes longos
plot_data_long <- bind_rows(
  df_it_long,
  df_m_long,
  df_imt_long
) %>%
  filter(!is.na(valor)) 

# Gerar o gráfico
g_painel <- ggplot(plot_data_long, aes(x = valor)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.8) +
  facet_wrap(~ variavel, scales = "free") + 
  labs(
    title = paste("Auditoria (DEPÓSITO A PRAZO): Distribuição das Variáveis Finais do Modelo (", data_inicio, "a", data_fim, ")"),
    subtitle = "Variáveis no formato exato usado na regressão (sem normalização).",
    x = "Valor da Variável (Logaritmizada ou Original)",
    y = "Contagem (Frequência)"
  ) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8))

# Salvar o gráfico
ggsave("AUDITORIA_PRAZO_HISTOGRAMAS.png", g_painel, width = 16, height = 9, dpi = 150)

print("--- AUDITORIA GERAL CONCLUÍDA ---")