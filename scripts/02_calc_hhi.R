# ==============================================================================
# SCRIPT 02: CÁLCULO DE HHI E VARIÁVEIS (Lógica Original Separada)
# Objetivo: Calcular shares e HHI usando exatamente a lógica do script BASE_ESTBAN.R
# Entrada: ESTBAN_LIMPA.rds
# Saída: ESTBAN_HHI.rds
# ==============================================================================

library(dplyr)
library(here)
library(readr)

# 1. Carregar a base limpa do passo anterior
caminho_entrada <- here("data", "processed", "ESTBAN_LIMPA.rds")
caminho_saida   <- here("data", "processed")

if (!file.exists(caminho_entrada)) {
  stop("Arquivo ESTBAN_LIMPA.rds não encontrado. Rode o Script 01 primeiro.")
}

print("Carregando base limpa...")
dados_filtrados <- readRDS(caminho_entrada)

print("Iniciando cálculos de HHI e Shares (Lógica Original)...")

# --- CÓDIGO ORIGINAL DO SEU SCRIPT (apenas adaptado para pegar o dataframe carregado) ---

dados_com_variaveis_calc <- dados_filtrados %>% 
  
  # 6.1. Calcular o total de depósitos no município (D_{mt})
  # Nota: A chave de agrupamento no seu script original era DATA_CALCULO e CODMUN_IBGE
  group_by(DATA_CALCULO, CODMUN_IBGE) %>%
  mutate(
    depositos_municipio_total = sum(deposito_prazo_itm, deposito_poupanca_itm, deposito_bancario_itm, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # 6.2. Calcular as variáveis de base e a participação de mercado
  mutate(
    log_ativos_it = log(pmax(ativos_itm, 0) + 1),
    total_depositos_banco = deposito_prazo_itm + deposito_poupanca_itm + deposito_bancario_itm,
    
    share_i = ifelse(
      depositos_municipio_total == 0, 0, (total_depositos_banco / depositos_municipio_total)
    ),
    share_i_quadrado = share_i^2
  ) %>%
  
  # 6.3. CALCULAR O hhi_m (HHI_{mt})
  group_by(DATA_CALCULO, CODMUN_IBGE) %>%
  mutate(
    hhi_m = sum(share_i_quadrado, na.rm = TRUE) 
  ) %>%
  ungroup() %>%
  
  # 6.4. MANTER APENAS AS ESSENCIAIS PARA OS PRÓXIMOS SCRIPTS
  select(DATA_BASE, CODMUN_IBGE, CNPJ, 
         deposito_prazo_itm, ativos_itm,
         log_ativos_it, hhi_m)

# ------------------------------------------------------------------------------

# Salvar o resultado final (para ser usado no Script 03 de Betas)
arquivo_final <- file.path(caminho_saida, "ESTBAN_HHI.rds")
saveRDS(dados_com_variaveis_calc, arquivo_final)

print(paste("--- Script 02 Concluído: HHI calculado e salvo em:", arquivo_final))