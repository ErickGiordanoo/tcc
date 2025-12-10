# ==============================================================================
# SETUP DO PROJETO: TCC PIX E COMPETIÇÃO BANCÁRIA
# Objetivo: Instalar dependências e garantir a estrutura de pastas.
# Execute este script APENAS UMA VEZ ou quando mudar de computador.
# ==============================================================================

# --- 1. INSTALAÇÃO DO GERENCIADOR DE PACOTES (PACMAN) -------------------------
# O 'pacman' verifica se o pacote existe: se não, instala; se sim, carrega.
if (!require("pacman")) {
  message("Instalando o pacote 'pacman' para gerenciamento de dependências...")
  install.packages("pacman")
}

# --- 2. LISTA DE PACOTES NECESSÁRIOS ------------------------------------------
# Aqui estão todos os pacotes usados nos scripts 01 a 06
pacotes <- c(
  # Manipulação de Dados (O núcleo do Tidyverse)
  "dplyr",        # Manipulação de tabelas (filter, select, mutate)
  "tidyr",        # Organização de dados (pivot_longer, pivot_wider)
  "stringr",      # Manipulação de textos
  "lubridate",    # Manipulação de datas
  "purrr",        # Programação funcional (map)
  "tibble",       # Dataframes modernos
  
  # Leitura e Escrita de Arquivos
  "readr",        # Ler CSVs rápidos
  "readxl",       # Ler Excel
  "writexl",      # Salvar Excel
  "here",         # Gerenciamento de caminhos relativos (CRUCIAL)
  
  # Econometria e Estatística
  "fixest",       # Modelos de Efeitos Fixos (Rápido e Robusto)
  "slider",       # Janelas móveis (Rolling Windows para os Betas)
  "broom",        # Limpar saídas de modelos estatísticos
  
  # Visualização e Tabelas
  "ggplot2",      # Gráficos
  "modelsummary", # Tabelas de regressão bonitas
  "gt",           # Tabelas descritivas elegantes
  "gtExtras"      # Temas extras para o gt
)

# --- 3. CARREGAR/INSTALAR PACOTES ---------------------------------------------
message("--- Verificando e instalando pacotes necessários... ---")
pacman::p_load(char = pacotes)
message("--- Todos os pacotes foram carregados com sucesso! ---")

# --- 4. GARANTIR ESTRUTURA DE PASTAS ------------------------------------------
# Cria as pastas automaticamente se elas não existirem
pastas_projeto <- c(
  "data/raw/estban",       # Onde ficam os CSVs do Bacen
  "data/raw/grouped",      # Onde ficam Pix e Macro
  "data/raw/originals",    # Onde ficam Municípios
  "data/processed",        # Onde o R salva os .rds
  "scripts",               # Seus códigos
  "outputs/figures",       # Seus gráficos
  "outputs/tables",        # Suas tabelas
  "outputs/models",        # Seus modelos salvos
  "docs"                   # Seus textos/PDFs
)

message("\n--- Verificando estrutura de diretórios... ---")
for (pasta in pastas_projeto) {
  if (!dir.exists(here(pasta))) {
    dir.create(here(pasta), recursive = TRUE)
    message(paste("Criada a pasta:", pasta))
  } else {
    # message(paste("Pasta já existe:", pasta)) # Comentado para não poluir
  }
}

message("\n==================================================================")
message(" SETUP CONCLUÍDO! SEU AMBIENTE ESTÁ PRONTO.")
message(" Próximo passo: Coloque os dados brutos nas pastas 'data/raw' e")
message(" execute os scripts na ordem (01 -> 02 -> 03...).")
message("==================================================================")