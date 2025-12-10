# ==============================================================================
# SCRIPT 01: ETL ESTBAN (Leitura e Limpeza - Lógica Original)
# Objetivo: Ler, limpar e aplicar dicionário.
# Saída: ESTBAN_LIMPA.rds
# ==============================================================================

library(dplyr)    
library(readr)    
library(writexl)  
library(stringr)
library(tidyr)
library(here) # Mantive o here para garantir os caminhos, mas a lógica é a sua
library(readxl)
library(purrr)

# 2. DEFINIR CAMINHOS
caminho_da_pasta_entrada <- here("data", "raw", "estban") 
caminho_da_pasta_saida   <- here("data", "processed")
caminho_dic              <- here("data", "raw", "originals", "municipios.xlsx")

if (!dir.exists(caminho_da_pasta_saida)) {
  dir.create(caminho_da_pasta_saida, recursive = TRUE)
}

# 3. LISTAR ARQUIVOS
print(paste("Procurando arquivos .csv em:", caminho_da_pasta_entrada))
arquivos_csv <- list.files(
  path = caminho_da_pasta_entrada, 
  pattern = "\\.csv$",        
  full.names = TRUE,
  ignore.case = TRUE, 
  recursive = TRUE    
)

# CNPJs a serem removidos (Mantendo comentado conforme seu original)
#cnpjs_a_remover <- c("03609817", "09516419", "01181521", "02038232", "23522214", "30723886", 
#                     "46518205", "09274232", "33042151", "58497702", "10264663")
cnpjs_a_remover <- c()

# 4. CARREGAR DICIONÁRIO DE CORREÇÃO
dicionario_correcao_dpara <- read_excel(caminho_dic) %>%
  select(
    MUNICIPIO_DPARA, 
    CODMUN_IBGE_CORRETO = CODMUN_IBGE_DPARA
  ) %>%
  mutate(
    MUNICIPIO_DPARA = toupper(str_squish(str_replace_all(str_replace_all(MUNICIPIO_DPARA, "-", " "), "[']", " ")))
  )

# 5. FUNÇÃO DE LEITURA (SEM O CÁLCULO DE HHI)
ler_limpar_selecionar_parcial <- function(caminho_arquivo) {
  
  # --- LEITURA E PREPARAÇÃO ---
  dados <- read_csv2(caminho_arquivo, locale = locale(encoding = "latin1"), show_col_types = FALSE) 
  
  # Renomeia os verbetes
  dados_renomeados <- dados
  if ("VERBETE_432_DEPOSITOS_A_PRAZO" %in% names(dados_renomeados)) {
    dados_renomeados <- rename(dados_renomeados, deposito_prazo_itm = VERBETE_432_DEPOSITOS_A_PRAZO)
  }
  if ("VERBETE_112_DEPOSITOS_BANCARIOS" %in% names(dados_renomeados)) {
    dados_renomeados <- rename(dados_renomeados, deposito_bancario_itm = VERBETE_112_DEPOSITOS_BANCARIOS)
  }
  if ("VERBETE_420_DEPOSITOS_DE_POUPANCA" %in% names(dados_renomeados)) {
    dados_renomeados <- rename(dados_renomeados, deposito_poupanca_itm = VERBETE_420_DEPOSITOS_DE_POUPANCA)
  }
  if ("VERBETE_399_TOTAL_DO_ATIVO" %in% names(dados_renomeados)) {
    dados_renomeados <- rename(dados_renomeados, ativos_itm = VERBETE_399_TOTAL_DO_ATIVO)
  }
  
  # --- CORREÇÃO DE AUSÊNCIA E TIPAGEM PARA CODMUN_IBGE e CODMUN ---
  if (!"CODMUN_IBGE" %in% names(dados_renomeados)) {
    dados_renomeados$CODMUN_IBGE <- NA_character_
  } else {
    dados_renomeados$CODMUN_IBGE <- as.character(dados_renomeados$CODMUN_IBGE)
  }
  
  if (!"CODMUN" %in% names(dados_renomeados)) {
    dados_renomeados$CODMUN <- NA_character_
  } else {
    dados_renomeados$CODMUN <- as.character(dados_renomeados$CODMUN)
  }
  
  # Filtra apenas as colunas de interesse
  dados_selecionados <- dados_renomeados %>%
    select(any_of(c(
      "DATA_BASE", "CODMUN_IBGE", "MUNICIPIO", "CNPJ", "NOME_INSTITUICAO",
      "deposito_prazo_itm", 
      "deposito_bancario_itm",  
      "deposito_poupanca_itm", 
      "ativos_itm", "#DATA_BASE", "CODMUN"
    )))
  
  # --- LIMPEZA GERAL E PADRONIZAÇÃO ---
  dados_limpos <- dados_selecionados %>%
    mutate(
      DATA_BASE_TEMP = if (exists("#DATA_BASE", where = .)) `#DATA_BASE` else DATA_BASE,
      DATA_BASE = str_pad(as.character(DATA_BASE_TEMP), 6, "left", "0"),
      
      # Importante: Mantendo DATA_CALCULO aqui pois é usada no próximo script
      DATA_CALCULO = as.Date(paste0(DATA_BASE, "01"), format = "%Y%m%d"),
      
      CODMUN_IBGE = coalesce(CODMUN_IBGE, CODMUN),
      CODMUN_IBGE = str_pad(str_trim(as.character(CODMUN_IBGE), "both"), 7, "left", "0"),
      
      across(
        any_of(c("MUNICIPIO", "NOME_INSTITUICAO")), 
        ~ toupper(str_squish(str_replace_all(str_replace_all(., "-", " "), "[']", " ")))
      ),
      CNPJ = str_pad(str_trim(as.character(CNPJ), "both"), 8, "left", "0"),
      
      # Padronização de variáveis numéricas
      across(
        any_of(c("deposito_prazo_itm", "deposito_bancario_itm", "deposito_poupanca_itm","ativos_itm")),
        ~ replace_na(as.numeric(gsub(",", ".", as.character(.))), 0)
      )
    ) %>%
    select(-any_of(c("#DATA_BASE", "CODMUN", "DATA_BASE_TEMP")))
  
  # --- APLICAÇÃO DOS DICIONÁRIOS ---
  dados_corrigidos_dpara <- dados_limpos %>%
    left_join(dicionario_correcao_dpara, by = c("MUNICIPIO" = "MUNICIPIO_DPARA")) %>%
    mutate(CODMUN_IBGE = coalesce(str_pad(CODMUN_IBGE_CORRETO, 7, "left", "0"), CODMUN_IBGE)) %>%
    select(-CODMUN_IBGE_CORRETO) 
  
  dados_filtrados <- dados_corrigidos_dpara %>%
    filter(!CNPJ %in% cnpjs_a_remover)
  
  # AQUI PARAMOS: Não calculamos HHI ainda. Retornamos a base limpa.
  return(dados_filtrados)
}

# 6. EXECUTAR PROCESSAMENTO
if (length(arquivos_csv) == 0) {
  stop("Nenhum arquivo .csv encontrado no diretório.")
} else {
  print(paste("Processando", length(arquivos_csv), "arquivos..."))
  
  dados_limpos_total <- map_dfr(arquivos_csv, ler_limpar_selecionar_parcial) %>% 
    distinct()
  
  # Salvar em RDS para passar para o script 02
  arquivo_rds <- file.path(caminho_da_pasta_saida, "ESTBAN_LIMPA.rds")
  saveRDS(dados_limpos_total, arquivo_rds)
  
  print("--- Script 01 Concluído: Dados limpos salvos em ESTBAN_LIMPA.rds ---")
}