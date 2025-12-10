# ==============================================================================
# SCRIPT 04: CONSOLIDAÇÃO DE DADOS (MERGE FINAL)
# Objetivo: Criar a base econométrica (Painel i-m-t) juntando todas as fontes.
# ==============================================================================

# --- 1. CARREGAR PACOTES ------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, readr, readxl, writexl, stringr, tidyr, here, lubridate)

# ==============================================================================
# 2. DEFINIR CAMINHOS E CONSTANTES
# ==============================================================================

# Inputs (Processados)
# Note: Agora lemos o ESTBAN_HHI (que tem os shares) e os BETAS
caminho_estban_hhi <- here("data", "processed", "ESTBAN_HHI.rds")
caminho_betas      <- here("data", "processed", "BASE_BETAS_FINAL.rds")

# Inputs (Brutos / Externos) - Ajuste se necessário
caminho_pix        <- here("data", "raw", "grouped", "BASE_PIX.xlsx")
caminho_macro      <- here("data", "raw", "grouped", "BASE_MACRO.xlsx")
caminho_municipios <- here("data", "raw", "grouped", "BASE_MUNICIPIOS.xlsx")

# Output
caminho_saida      <- here("data", "processed")

# Configurações do Estudo
CNPJS_GRANDES <- c("00000000", "00360305") # BB e Caixa
DATA_HHI_FIXO <- "202010"                  # Data de corte exógena

# ==============================================================================
# 3. CARREGAMENTO
# ==============================================================================

print("--- 1. Carregando bases... ---")

# Leitura segura
if (!file.exists(caminho_estban_hhi)) stop("ESTBAN_HHI.rds não encontrado. Rode o Script 02.")
if (!file.exists(caminho_betas)) stop("BASE_BETAS_FINAL.rds não encontrado. Rode o Script 03.")

base_bancaria <- readRDS(caminho_estban_hhi)
base_betas    <- readRDS(caminho_betas)

# Dados externos
raw_pix        <- read_excel(caminho_pix)
raw_macro      <- read_excel(caminho_macro)
raw_municipios <- read_excel(caminho_municipios)

# ==============================================================================
# 5. PADRONIZAÇÃO PARA JOIN
# ==============================================================================

print("--- 3. Padronizando chaves... ---")

# Principal
df_main <- base_bancaria %>%
  rename(data = DATA_BASE, cod_mun = CODMUN_IBGE, cnpj = CNPJ)

# Pix
df_pix <- raw_pix %>%
  rename(data = DATA_BASE, cod_mun = CODMUN_IBGE, valor_pix_mt = VR_PIX) %>%
  mutate(
    data = as.character(data),
    cod_mun = str_pad(as.character(cod_mun), 7, "left", "0")
  )

# Macro
df_macro <- raw_macro %>%
  rename(data = DATA_BASE) %>%
  mutate(data = as.character(data))

# Municípios
df_municipios <- raw_municipios %>%
  rename(
    cod_mun = CODMUN_IBGE,
    populacao_m = populacao,
    renda_per_capita_m = renda_per_capita,
    pib_per_capita_m = pib_per_capita
  ) %>%
  mutate(cod_mun = str_pad(as.character(cod_mun), 7, "left", "0")) %>%
  select(
    cod_mun, populacao_m, pib_per_capita_m, 
    renda_per_capita_m, share_populacao_rural, 
    share_populacao_feminina, share_populacao_jovem, tx_analfabeto
  )

# ==============================================================================
# 6. CRIAÇÃO DO HHI FIXO (INSTRUMENTO)
# ==============================================================================

print("--- 4. Criando HHI Fixo (Out/2020)... ---")

hhi_fixo <- df_main %>%
  filter(data == DATA_HHI_FIXO) %>% 
  distinct(cod_mun, hhi_m) %>% 
  rename(hhi_m_fixo = hhi_m) %>%
  filter(!is.na(hhi_m_fixo))

# ==============================================================================
# 7. MERGE FINAL E CRIAÇÃO DE VARIÁVEIS
# ==============================================================================

print("--- 5. Executando Merge e Engenharia de Variáveis... ---")

base_final <- df_main %>%
  # Joins
  left_join(hhi_fixo, by = "cod_mun") %>%
  left_join(df_pix, by = c("cod_mun", "data")) %>%
  left_join(df_macro, by = "data") %>%
  left_join(df_municipios, by = "cod_mun") %>%
  inner_join(base_betas_limpa, by = c("cnpj", "data")) %>% # Inner join remove quem não tem beta
  
  # Criação de Variáveis
  mutate(
    # Pix (Log)
    log_pix_mt = log(1 + replace_na(valor_pix_mt, 0)),
    
    # Dummy Small Bank (Si)
    Si = if_else(cnpj %in% CNPJS_GRANDES, 0, 1),
    
    # Interação (Tratamento)
    pix_x_Si = log_pix_mt * Si,
    
    # Controles (Logs)
    log_pib_per_capita_m   = log(pmax(pib_per_capita_m, 0) + 1),
    log_renda_per_capita_m = log(pmax(renda_per_capita_m, 0) + 1),
    log_populacao_m        = log(pmax(populacao_m, 0) + 1)
  )

# ==============================================================================
# 8. EXPORTAÇÃO
# ==============================================================================

print("--- 6. Salvando Base Final... ---")

arquivo_final <- file.path(caminho_saida, "BASE_FINAL_MODELO.rds")
saveRDS(base_final, arquivo_final)

print(paste("Base pronta para modelagem salva em:", arquivo_final))
print(glimpse(base_final))