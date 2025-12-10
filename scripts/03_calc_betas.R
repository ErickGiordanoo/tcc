# ==============================================================================
# SCRIPT 03: CÁLCULO DOS BETAS (METODOLOGIA SARKISYAN)
# Objetivo: Estimar a sensibilidade dos depósitos (Beta) em relação à Selic.
# Metodologia:
#   1. Agregação: Soma dos depósitos de todas as agências (Nível Banco).
#   2. Janela: 10 meses (Rolling Window).
#   3. Modelo: Semi-elasticidade (Delta Log Dep ~ Delta Nível Selic).
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, readxl, tidyr, purrr, slider, here, stringr, lubridate)

# --- 1. CONFIGURAÇÃO ----------------------------------------------------------
caminho_estban_hhi <- here("data", "processed", "ESTBAN_HHI.rds")
caminho_macro      <- here("data", "raw", "grouped", "BASE_MACRO.xlsx") 
caminho_saida      <- here("data", "processed")

TAMANHO_JANELA <- 10

# --- 2. CARREGAMENTO E LIMPEZA ------------------------------------------------
print("--- 1. Carregando dados... ---")
base_estban <- readRDS(caminho_estban_hhi)
raw_macro   <- read_excel(caminho_macro)

# Função de limpeza de data
limpar_data <- function(x) { str_sub(str_remove_all(as.character(x), "[^0-9]"), 1, 6) }

# Macroeconomia (Selic)
base_macro <- raw_macro %>%
  mutate(DATA_BASE = limpar_data(DATA_BASE)) %>%
  group_by(DATA_BASE) %>%
  summarise(selic = mean(as.numeric(selic), na.rm=TRUE)) %>%
  arrange(DATA_BASE) %>%
  fill(selic, .direction = "down") # Garante continuidade

# --- 3. AGREGAÇÃO POR BANCO (CRUCIAL) -----------------------------------------
print("--- 2. Agregando por Banco (Soma Nacional)... ---")

# Sarkisyan calcula o beta do BANCO, não da agência.
df_banco_mes <- base_estban %>%
  mutate(DATA_BASE = limpar_data(DATA_BASE)) %>%
  group_by(CNPJ, DATA_BASE) %>%
  summarise(
    # Soma de todas as agências do banco no país
    deposito_prazo_it = sum(deposito_prazo_itm, na.rm = TRUE),
    .groups = 'drop'
  )

# Join com Macro e Deltas
df_deltas <- df_banco_mes %>%
  inner_join(base_macro, by = "DATA_BASE") %>%
  group_by(CNPJ) %>%
  arrange(DATA_BASE) %>%
  mutate(
    # Y: Log Growth (Sarkisyan Tabela 3: "Deposit Growth")
    log_prazo       = log(deposito_prazo_it + 1),
    delta_log_prazo = log_prazo - lag(log_prazo, 1),
    
    # X: Change in Selic (Level)
    delta_selic     = selic - lag(selic, 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(delta_log_prazo), !is.na(delta_selic))

# --- 4. CÁLCULO DOS BETAS (JANELA MÓVEL) --------------------------------------
print("--- 3. Rodando Regressões em Janela Móvel... ---")

calcular_beta_sarkisyan <- function(df) {
  # Precisa de dados suficientes na janela
  if (nrow(df) < 8) return(NA_real_)
  
  # Se Selic não mudou (Platô), a sensibilidade via taxa é tecnicamente zero.
  # Retornar 0 preserva o banco na amostra durante períodos de estabilidade.
  if (var(df$delta_selic, na.rm = TRUE) < 1e-8) return(0)
  
  # Regressão Univariada (Growth ~ Rate Change)
  # Isso evita o problema de multicolinearidade em janelas curtas
  tryCatch({
    mod <- lm(delta_log_prazo ~ delta_selic, data = df)
    return(coef(mod)["delta_selic"])
  }, error = function(e) return(NA_real_))
}

base_betas <- df_deltas %>%
  group_by(CNPJ) %>%
  arrange(DATA_BASE) %>%
  mutate(
    b_it_prazo = slide_dbl(
      .x = cur_data(), 
      .f = calcular_beta_sarkisyan, 
      .before = TAMANHO_JANELA - 1, 
      .complete = FALSE 
    )
  ) %>%
  ungroup()

# --- 5. RESULTADO -------------------------------------------------------------
base_betas_final <- base_betas %>%
  select(CNPJ, DATA_BASE, b_it_prazo) %>%
  filter(!is.na(b_it_prazo))

print(paste("Total de Betas (Banco-Mês) calculados:", nrow(base_betas_final)))
print("Estatísticas Descritivas (Isso deve parecer com Flow Betas):")
print(summary(base_betas_final$b_it_prazo))

# Salvar
saveRDS(base_betas_final, file.path(caminho_saida, "BASE_BETAS_FINAL.rds"))
print("--- Script 03 Concluído ---")