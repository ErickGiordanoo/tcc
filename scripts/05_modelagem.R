# ==============================================================================
# SCRIPT 05: MODELAGEM ECONOMÉTRICA (EFEITOS FIXOS) - VERSÃO CORRIGIDA
# Objetivo: Estimar o modelo DiD com Efeitos Fixos.
# Correção: Tratamento automático para HHI Fixo vazio.
# ==============================================================================

# --- 1. CARREGAR PACOTES ------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(fixest, dplyr, readr, tidyr, here)

# ==============================================================================
# 2. DEFINIR CAMINHOS E PARÂMETROS
# ==============================================================================

path_entrada <- here("data", "processed", "BASE_FINAL_MODELO.rds")
path_saida   <- here("outputs", "models")

if (!dir.exists(path_saida)) dir.create(path_saida, recursive = TRUE)

# Período de Análise
DATA_INICIO <- "202011"
DATA_FIM    <- "202507"

# ==============================================================================
# 3. PREPARAÇÃO E CORREÇÃO DOS DADOS
# ==============================================================================

print("--- 1. Carregando dados... ---")

if (!file.exists(path_entrada)) stop("Base final não encontrada.")
base_bruta <- readRDS(path_entrada)

print("--- 2. Corrigindo HHI Fixo e Filtrando... ---")

base_regressao <- base_bruta %>%
  # CORREÇÃO DO HHI:
  # Se o hhi_m_fixo veio vazio do merge, vamos pegar o primeiro hhi_m disponível do município
  group_by(cod_mun) %>%
  arrange(data) %>%
  mutate(
    # Tenta usar o fixo existente, se for NA, pega o primeiro valor válido de hhi_m do município
    hhi_m_fixo = ifelse(is.na(hhi_m_fixo), first(na.omit(hhi_m)), hhi_m_fixo)
  ) %>%
  ungroup() %>%
  
  # Filtro Temporal (como texto para garantir)
  filter(data >= DATA_INICIO & data <= DATA_FIM) %>%
  
  # Remoção de NAs (Blindada)
  # Removemos apenas se as variáveis CRUCIAIS forem NA.
  # Se faltar controle demográfico (0.02% dos casos), preenchemos com a média para não perder o banco.
  mutate(
    # Preencher controles demográficos faltantes com a média da amostra (Inputação simples)
    log_populacao_m        = ifelse(is.na(log_populacao_m), mean(log_populacao_m, na.rm=T), log_populacao_m),
    log_pib_per_capita_m   = ifelse(is.na(log_pib_per_capita_m), mean(log_pib_per_capita_m, na.rm=T), log_pib_per_capita_m),
    log_renda_per_capita_m = ifelse(is.na(log_renda_per_capita_m), mean(log_renda_per_capita_m, na.rm=T), log_renda_per_capita_m),
    # Pix pode ser 0 se for NA
    log_pix_mt = ifelse(is.na(log_pix_mt), 0, log_pix_mt),
    pix_x_Si   = ifelse(is.na(pix_x_Si), 0, pix_x_Si)
  ) %>%
  # Agora sim, removemos apenas se o BETA ou o GRUPO (Si) forem nulos, pois sem Y não há regressão.
  filter(!is.na(b_it_prazo), !is.na(Si))

print(paste("Número de observações FINAIS para o modelo:", nrow(base_regressao)))

if (nrow(base_regressao) < 100) {
  stop("ERRO: A base continua vazia ou muito pequena. Verifique se os dados de Beta foram calculados corretamente.")
}

# ==============================================================================
# 4. ESTIMAÇÃO DO MODELO (FIXEST)
# ==============================================================================

print("--- 3. Estimando Modelo de Efeitos Fixos (TWFE)... ---")

# Fórmula (usando o hhi_m_fixo que acabamos de corrigir)
formula_twfe <- as.formula(
  b_it_prazo ~ 
    log_pix_mt + pix_x_Si + hhi_m_fixo + 
    log_ativos_it + log_populacao_m + log_pib_per_capita_m + 
    log_renda_per_capita_m + share_populacao_rural + 
    share_populacao_feminina + share_populacao_jovem + tx_analfabeto
  | cnpj + data 
)

# Estimação
modelo_fe <- feols(
  fml = formula_twfe,
  data = base_regressao,
  cluster = "cod_mun"
)

# ==============================================================================
# 5. SALVAMENTO E RESUMO
# ==============================================================================

print("--- 4. Resultado da Regressão: ---")
print(modelo_fe)

# Salvar
arquivo_modelo <- file.path(path_saida, "modelo_principal_fe.rds")
saveRDS(modelo_fe, arquivo_modelo)

print(paste("Modelo salvo em:", arquivo_modelo))
print("--- SCRIPT 05 CONCLUÍDO COM SUCESSO ---")