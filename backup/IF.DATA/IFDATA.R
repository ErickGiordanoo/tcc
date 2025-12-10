library(data.table)
library(stringr)
library(writexl) # <--- NOVO PACOTE ADICIONADO para salvar em .xlsx

# --- 1. CONFIGURAÇÃO ---
# Coloque o caminho para a pasta onde estão todos os seus arquivos CSV "quebrados"
pasta_dos_arquivos <- "G:/TCC/DADOS/IF.DATA"

# --- 2. LISTAR OS ARQUIVOS ---
# Encontra todos os arquivos que terminam com .csv na pasta
arquivos_para_processar <- list.files(
  path = pasta_dos_arquivos,
  pattern = "\\.csv$", # O padrão é "termina com .csv"
  full.names = TRUE    # Queremos o caminho completo do arquivo
)

print(paste("Encontrados", length(arquivos_para_processar), "arquivos para processar."))

# --- 3. FUNÇÃO MÁGICA DE CORREÇÃO ---
# (Esta função está idêntica à sua, não mudei nada)
processar_arquivo <- function(caminho_do_arquivo) {
  
  # Lê o arquivo como linhas de texto, não como um CSV
  linhas_raw <- readLines(caminho_do_arquivo, warn = FALSE)
  
  # Pega o cabeçalho (Linha 1)
  cabecalho <- linhas_raw[1]
  
  # Pega todas as outras linhas (os dados)
  linhas_de_dados <- linhas_raw[-1]
  
  # ----- Esta é a lógica principal -----
  linhas_corrigidas <- c() # Um vetor vazio para guardar as linhas consertadas
  buffer_de_linha <- ""    # Um "buffer" para juntar linhas quebradas
  
  for (linha_atual in linhas_de_dados) {
    # Junta a linha atual ao buffer
    buffer_de_linha <- paste0(buffer_de_linha, linha_atual)
    
    # VERIFICAÇÃO: A linha está completa?
    # Nossa regra: uma linha está completa se tiver um número PAR de aspas (")
    if (str_count(buffer_de_linha, '"') %% 2 == 0) {
      # O número de aspas é par! A linha está completa.
      # Adicionamos ao nosso vetor de linhas corrigidas
      linhas_corrigidas <- c(linhas_corrigidas, buffer_de_linha)
      
      # EsvaziAMOS o buffer para a próxima linha
      buffer_de_linha <- ""
    } else {
      # O número de aspas é ímpar.
      # A linha está quebrada. Não fazemos nada,
      # e o loop continua para a próxima linha,
      # que será adicionada a este mesmo buffer.
    }
  }
  # ----- Fim da lógica principal -----
  
  # Se o loop terminar e ainda houver algo no buffer, é um erro no arquivo
  if (buffer_de_linha != "") {
    warning(paste("Arquivo", basename(caminho_do_arquivo), "terminou com uma linha incompleta."))
    return(NULL) # Retorna nulo para este arquivo
  }
  
  # Se não tivermos linhas corrigidas, retorna nulo
  if (length(linhas_corrigidas) == 0) {
    return(NULL)
  }
  
  # Agora temos o cabeçalho e as linhas de dados corrigidas.
  # Vamos juntar tudo em um único bloco de texto
  texto_completo_corrigido <- paste(c(cabecalho, linhas_corrigidas), collapse = "\n")
  
  # Usamos fread(text=...) para ler o TEXTO que acabamos de montar
  # A função fread é da biblioteca data.table e é muito rápida
  dados_corrigidos <- fread(text = texto_completo_corrigido)
  
  # (Opcional) Adiciona uma coluna para saber de qual arquivo veio a linha
  dados_corrigidos[, source_file := basename(caminho_do_arquivo)]
  
  return(dados_corrigidos)
}


# --- 4. EXECUTAR E CONSOLIDAR ---

# (Esta seção também está idêntica à sua)
lista_de_data_tables <- lapply(arquivos_para_processar, function(arquivo) {
  tryCatch(
    {
      print(paste("Processando:", basename(arquivo)))
      processar_arquivo(arquivo)
    },
    error = function(e) {
      # Se der erro, imprime o erro e continua
      print(paste("ERRO ao processar", basename(arquivo), ":", e$message))
      return(NULL) # Retorna nulo para o arquivo com erro
    }
  )
})

# Remove qualquer 'NULL' da lista (arquivos que falharam ou estavam vazios)
lista_de_data_tables <- Filter(Negate(is.null), lista_de_data_tables)

# Agora consolidamos todos os data.tables da lista em um único
# data.table gigante!
dados_consolidados <- rbindlist(lista_de_data_tables)

print("Consolidação concluída!")


# --- 5. FILTRAR E SALVAR (AQUI ESTÁ A MUDANÇA!) ---

# Primeiro, definimos os valores que queremos manter na 'NomeColuna'
# Esta lista é baseada nos seus requisitos.
valores_para_filtrar <- c(
  "Ativo Total",
  "Depósitos à Vista (a1)",
  "Depósitos de Poupança (a2)",
  "Depósitos a Prazo (a4)"
)

# Agora, filtramos os dados.
# Usamos 'grepl' com um 'paste' para criar uma lógica "OU"
# Isso garante que vamos pegar "Ativo Total (k) = (i) - (j)"
# e também "Ativo Total" (se ele vier sem o sufixo).
# Basicamente, filtramos por "linhas que COMEÇAM com..."
padroes_regex <- paste0("^", str_escape(valores_para_filtrar), collapse = "|")

print(paste("Filtrando por:", paste(valores_para_filtrar, collapse = ", ")))
dados_filtrados <- dados_consolidados[grepl(padroes_regex, NomeColuna)]

print("Filtragem concluída!")
print(dados_filtrados)


# Finalmente, salvamos os dados FILTRADOS em .xlsx
caminho_saida_xlsx <- "G:/TCC/DADOS/MODELO/BASE_IFDATA.xlsx"

print(paste("Salvando arquivo .xlsx em:", caminho_saida_xlsx))

# Usamos a função write_xlsx (do pacote 'writexl')
# ela é rápida e salva no formato Excel corretamente.
write_xlsx(dados_filtrados, caminho_saida_xlsx)

print("SUCESSO! Arquivo salvo.")
