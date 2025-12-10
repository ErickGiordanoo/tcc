# ==============================================================================
# SCRIPT 05: GERAÇÃO DE TABELAS E GRÁFICOS
# Objetivo: Produzir os outputs visuais finais para o TCC.
# ==============================================================================

# --- 1. CARREGAR PACOTES ------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, readr, tidyr, ggplot2, here, 
  gt, gtExtras, modelsummary, fixest, tibble, lubridate
)

# ==============================================================================
# 2. CONFIGURAÇÃO E CARREGAMENTO
# ==============================================================================

# Caminhos
path_dados  <- here("data", "processed", "BASE_FINAL_MODELO.rds")
path_modelo <- here("outputs", "models", "modelo_principal_fe.rds")

# Pastas de Saída (Cria se não existirem)
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

# Carregar Dados
print("--- Carregando dados e modelo... ---")
if (!file.exists(path_dados)) stop("Base de dados não encontrada.")
if (!file.exists(path_modelo)) stop("Modelo não encontrado. Rode o Script 04.")

base_final <- readRDS(path_dados)
modelo_fe  <- readRDS(path_modelo)

# Filtro de Período (O mesmo do TCC)
DATA_INICIO <- "202011"
DATA_FIM    <- "202507"

base_analise <- base_final %>%
  filter(data >= DATA_INICIO & data <= DATA_FIM)

# ==============================================================================
# 3. TABELA 1: ESTATÍSTICAS DESCRITIVAS (CONTEXTO)
# ==============================================================================
print("--- Gerando Tabela 1... ---")

# Preparação dos dados agregados
# Nível Mês (t)
stats_macro <- base_analise %>%
  distinct(data, .keep_all = TRUE) %>%
  summarise(
    inflacao_m = mean(IPCA * 100, na.rm = T), inflacao_sd = sd(IPCA * 100, na.rm = T),
    desemp_m   = mean(PNADC * 100, na.rm = T), desemp_sd   = sd(PNADC * 100, na.rm = T),
    selic_m    = mean(selic * 100, na.rm = T), selic_sd    = sd(selic * 100, na.rm = T),
    cambio_m   = mean(CAMBIO, na.rm = T),      cambio_sd   = sd(CAMBIO, na.rm = T)
  )

# Nível Município-Mês (m-t)
stats_pix <- base_analise %>%
  distinct(cod_mun, data, .keep_all = TRUE) %>%
  summarise(
    pix_m  = mean(valor_pix_mt / 1e6, na.rm = T), # Em Milhões
    pix_sd = sd(valor_pix_mt / 1e6, na.rm = T)
  )

# Nível Município (m) - Variáveis fixas/anuais
stats_muni <- base_analise %>%
  distinct(cod_mun, .keep_all = TRUE) %>%
  summarise(
    pop_m   = mean(populacao_m / 1e3, na.rm = T), pop_sd = sd(populacao_m / 1e3, na.rm = T),
    jovem_m = mean(share_populacao_jovem * 100, na.rm = T), jovem_sd = sd(share_populacao_jovem * 100, na.rm = T),
    fem_m   = mean(share_populacao_feminina * 100, na.rm = T), fem_sd = sd(share_populacao_feminina * 100, na.rm = T),
    rural_m = mean(share_populacao_rural * 100, na.rm = T), rural_sd = sd(share_populacao_rural * 100, na.rm = T),
    analf_m = mean(tx_analfabeto * 100, na.rm = T), analf_sd = sd(tx_analfabeto * 100, na.rm = T),
    pib_m   = mean(pib_per_capita_m / 1e3, na.rm = T), pib_sd = sd(pib_per_capita_m / 1e3, na.rm = T),
    hhi_m   = mean(hhi_m_fixo, na.rm = T), hhi_sd = sd(hhi_m_fixo, na.rm = T)
  )

# Montagem do Tibble
tb1_dados <- tibble(
  Painel = c(rep("A: Pix (Município-Mês)", 1), rep("B: Município", 7), rep("C: Macro (Mês)", 4)),
  Variavel = c(
    "Total Transação Pix (Milhões R$)",
    "População (Milhares)", "% Jovens", "% Mulheres", "% Rural", "% Analfabetismo", "PIB per capita (Milhares R$)", "HHI (Out/2020)",
    "Inflação (%)", "Desemprego (%)", "Selic (%)", "Câmbio (R$)"
  ),
  Media = c(
    stats_pix$pix_m,
    stats_muni$pop_m, stats_muni$jovem_m, stats_muni$fem_m, stats_muni$rural_m, stats_muni$analf_m, stats_muni$pib_m, stats_muni$hhi_m,
    stats_macro$inflacao_m, stats_macro$desemp_m, stats_macro$selic_m, stats_macro$cambio_m
  ),
  DesvPad = c(
    stats_pix$pix_sd,
    stats_muni$pop_sd, stats_muni$jovem_sd, stats_muni$fem_sd, stats_muni$rural_sd, stats_muni$analf_sd, stats_muni$pib_sd, stats_muni$hhi_sd,
    stats_macro$inflacao_sd, stats_macro$desemp_sd, stats_macro$selic_sd, stats_macro$cambio_sd
  )
)

# Renderização GT
tabela1 <- tb1_dados %>%
  gt(groupname_col = "Painel") %>%
  cols_label(Media = "Média", DesvPad = "Desv. Padrão") %>%
  fmt_number(columns = c(Media, DesvPad), decimals = 2) %>%
  tab_header(title = md("**Tabela 1: Estatísticas Descritivas**")) %>%
  tab_source_note("Fonte: Elaboração própria com dados do BCB e IBGE.")

gtsave(tabela1, here("outputs", "tables", "TABELA_1_DESCRITIVA.png"))

# ==============================================================================
# 4. TABELA 2: COMPARATIVA (GRANDES VS PEQUENOS)
# ==============================================================================
print("--- Gerando Tabela 2... ---")

# Preparação
tb2_dados <- base_analise %>%
  mutate(Grupo = ifelse(Si == 0, "Grandes Bancos", "Bancos Pequenos")) %>%
  group_by(Grupo) %>%
  summarise(
    Beta_Media = mean(b_it_prazo, na.rm = T), Beta_SD = sd(b_it_prazo, na.rm = T),
    Pix_Media  = mean(log_pix_mt, na.rm = T), Pix_SD  = sd(log_pix_mt, na.rm = T),
    HHI_Media  = mean(hhi_m_fixo, na.rm = T), HHI_SD  = sd(hhi_m_fixo, na.rm = T),
    Ativ_Media = mean(log_ativos_it, na.rm = T), Ativ_SD = sd(log_ativos_it, na.rm = T)
  ) %>%
  pivot_longer(-Grupo, names_to = c("Var", "Stat"), names_sep = "_") %>%
  pivot_wider(names_from = c(Grupo, Stat), values_from = value)

# Mapeamento de nomes
mapa_vars <- c("Beta" = "Beta de Depósito", "Pix" = "Log(Pix)", "HHI" = "HHI Pré-Pix", "Ativ" = "Log(Ativos)")
tb2_dados$Var <- mapa_vars[tb2_dados$Var]

# Renderização GT
tabela2 <- tb2_dados %>%
  gt() %>%
  tab_spanner(label = md("**Grandes Bancos (Si=0)**"), columns = starts_with("Grandes")) %>%
  tab_spanner(label = md("**Bancos Pequenos (Si=1)**"), columns = starts_with("Bancos")) %>%
  cols_label(
    Var = "Variável",
    `Grandes Bancos_Media` = "Média", `Grandes Bancos_SD` = "D.P.",
    `Bancos Pequenos_Media` = "Média", `Bancos Pequenos_SD` = "D.P."
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 3) %>%
  tab_header(title = md("**Tabela 2: Comparativo por Grupo de Banco**"))

gtsave(tabela2, here("outputs", "tables", "TABELA_2_COMPARATIVA.png"))

# ==============================================================================
# 5. GRÁFICO 1: EVOLUÇÃO DOS BETAS (SÉRIE TEMPORAL)
# ==============================================================================
print("--- Gerando Gráfico 1... ---")

# Prepara dados diários para o plot
plot_data <- base_analise %>%
  mutate(
    Grupo = factor(ifelse(Si == 0, "Grandes Bancos", "Bancos Pequenos")),
    data_date = ym(data) # converte "202010" para data real
  ) %>%
  group_by(Grupo, data_date) %>%
  summarise(beta_medio = mean(b_it_prazo, na.rm = TRUE), .groups = 'drop')

grafico1 <- ggplot(plot_data, aes(x = data_date, y = beta_medio, color = Grupo)) +
  geom_vline(xintercept = as.Date("2020-11-01"), linetype = "dashed", color = "gray50") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  annotate("text", x = as.Date("2021-06-01"), y = max(plot_data$beta_medio), 
           label = "Início do Pix", color = "gray40", size = 3) +
  scale_color_manual(values = c("Grandes Bancos" = "#d1495b", "Bancos Pequenos" = "#00798c")) +
  labs(
    title = "Gráfico 1: Evolução do Beta de Depósito Médio",
    subtitle = "Comparação da sensibilidade à Selic entre grupos (2020-2025)",
    x = "Data", y = "Beta de Depósito (Média)", color = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave(here("outputs", "figures", "GRAFICO_1_EVOLUCAO_BETAS.png"), grafico1, width = 10, height = 6)

# ==============================================================================
# 6. TABELA 3: RESULTADOS DA REGRESSÃO
# ==============================================================================
print("--- Gerando Tabela 3... ---")

# Dicionário de Variáveis
coef_map <- c(
  "log_pix_mt" = "Pix (Log)",
  "pix_x_Si"   = "Pix * Banco Pequeno",
  "hhi_m_fixo" = "HHI (Controle)"
)

# Renderização
tabela3 <- modelsummary(
  modelo_fe,
  output = "gt",
  coef_map = coef_map,
  stars = TRUE,
  gof_map = list(
    list("raw" = "nobs", "clean" = "Observações", "fmt" = 0),
    list("raw" = "r.squared.within", "clean" = "R² (Within)", "fmt" = 3)
  ),
  title = md("**Tabela 3: Modelo de Efeitos Fixos (Resultados)**")
) %>%
  tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = 3)) # Destaque na interação

gtsave(tabela3, here("outputs", "tables", "TABELA_3_REGRESSAO.png"))

# ==============================================================================
# 7. GRÁFICO 2: COEFICIENTES (EFEITO LÍQUIDO)
# ==============================================================================
print("--- Gerando Gráfico 2... ---")

# Extrair coeficientes do modelo real
coefs <- coefficients(modelo_fe)
beta_pix      <- coefs["log_pix_mt"]
beta_interacao <- coefs["pix_x_Si"]

# Calcular efeitos
efeito_grandes  <- beta_pix
efeito_pequenos <- beta_pix + beta_interacao

df_barras <- tibble(
  Grupo = factor(c("Grandes Bancos", "Bancos Pequenos"), levels = c("Grandes Bancos", "Bancos Pequenos")),
  Efeito = c(efeito_grandes, efeito_pequenos),
  Label = round(c(efeito_grandes, efeito_pequenos), 4)
)

grafico2 <- ggplot(df_barras, aes(x = Grupo, y = Efeito, fill = Grupo)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = Label), vjust = ifelse(df_barras$Efeito > 0, -0.5, 1.5), fontface = "bold") +
  scale_fill_manual(values = c("Grandes Bancos" = "#d1495b", "Bancos Pequenos" = "#00798c")) +
  labs(
    title = "Gráfico 2: Efeito Líquido do Pix na Elasticidade de Depósitos",
    subtitle = "Variação no Beta para cada grupo (Baseado no Modelo FE)",
    y = "Variação no Beta (Coeficiente)", x = NULL
  ) +
  theme_bw() +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave(here("outputs", "figures", "GRAFICO_2_BARRA_COEFICIENTES.png"), grafico2, width = 8, height = 6)

print("--- TODOS OS OUTPUTS FORAM GERADOS COM SUCESSO! ---")