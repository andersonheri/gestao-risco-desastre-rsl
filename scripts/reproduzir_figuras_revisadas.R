args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
if (length(script_arg) != 1) stop("Execute este arquivo com Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
data_path <- file.path(project_root, "dados", "banco_final.xlsx")
output_dir <- file.path(project_root, "resultados", "figuras")
audit_dir <- file.path(project_root, "resultados", "auditoria")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(readxl)
})

df <- read_excel(data_path)
names(df) <- make.names(names(df), unique = TRUE)

# ============================================================
# DICIONARIO DE CLASSIFICACAO EM TRES NIVEIS
# Nivel 1: Estrategia/desenho de pesquisa
# Nivel 2: Tecnica de analise de dados
# Nivel 3: Instrumento de coleta de dados
# ============================================================
# Chave = valor bruto (minusculo, sem espacos extras) tal como aparece
# nas colunas 'quantitativa' e 'qualitativa' apos split por virgula.
dicionario <- tribble(
  ~valor_bruto,                        ~nivel,        ~categoria_padronizada,
  "estatistica descritiva",            "tecnica",     "Estatística Descritiva",
  "estatística descritiva",            "tecnica",     "Estatística Descritiva",
  "análise espacial",                  "tecnica",     "Análise Espacial",
  "análise fatorial",                  "tecnica",     "Análise Fatorial",
  "matrizes decisórias",               "tecnica",     "Matrizes Decisórias",
  "análise de conteúdo",               "tecnica",     "Análise de Conteúdo",
  "análise de conteudo",               "tecnica",     "Análise de Conteúdo",
  "análise do discurso",               "tecnica",     "Análise do Discurso",
  "análise discursiva",                "tecnica",     "Análise do Discurso",
  "análise de discurso",               "tecnica",     "Análise do Discurso",
  "análise de causa raiz",             "tecnica",     "Análise de Causa Raiz",
  "análise documental",                "tecnica",     "Análise Documental",
  "análise documenta",                 "tecnica",     "Análise Documental",
  "estudo de caso",                    "estrategia",  "Estudo de Caso",
  "etnografia",                        "estrategia",  "Etnografia",
  "pesquisa-ação",                     "estrategia",  "Pesquisa-ação",
  "revisão sistemática",               "estrategia",  "Revisão da Literatura",
  "revisão bibliográfica",             "estrategia",  "Revisão da Literatura",
  "revisão da literatura",             "estrategia",  "Revisão da Literatura",
  "revisão de literatura",             "estrategia",  "Revisão da Literatura",
  "entrevistas",                       "instrumento", "Entrevista",
  "entrevistas semiestruturadas",      "instrumento", "Entrevista Semiestruturada",
  "entrevistas semiestruturada",       "instrumento", "Entrevista Semiestruturada",
  "observação participante",           "instrumento", "Observação Participante"
)

# ---- Funcao para separar, limpar e classificar uma coluna multi-valorada ----
classificar_coluna <- function(data, coluna) {
  data %>%
    rename(valor = all_of(coluna)) %>%
    mutate(valor = str_to_lower(valor)) %>%
    separate_rows(valor, sep = ",|\\se\\s") %>%
    mutate(valor = str_trim(valor)) %>%
    filter(valor != "" & valor != "na") %>%
    # normalizacoes pontuais de grafia antes do join
    mutate(valor = recode(valor,
      "análise documental e análise de causa raiz" = "análise documental",
      "estudo de caso e análise documental" = "estudo de caso",
      .default = valor
    )) %>%
    left_join(dicionario, by = c("valor" = "valor_bruto"))
}

df <- df %>% mutate(ID_Publicacao = row_number())

qual_classif <- classificar_coluna(df %>% select(ID_Publicacao, qualitativa), "qualitativa")
quant_classif <- classificar_coluna(df %>% select(ID_Publicacao, quantitativa), "quantitativa")

nao_mapeados <- bind_rows(qual_classif, quant_classif) %>% filter(is.na(categoria_padronizada))
cat("=== Valores NAO mapeados no dicionario (revisar) ===\n")
print(unique(nao_mapeados$valor))

classif_total <- bind_rows(qual_classif, quant_classif) %>% filter(!is.na(categoria_padronizada))

cat("\n=== N de estudos por NIVEL (contagem de mencoes, nao de estudos unicos) ===\n")
print(classif_total %>% count(nivel))

# Frequencias por nivel (percentual sobre estudos com informacao no nivel, N=50 base)
freq_nivel <- function(dados, nivel_alvo, titulo, arquivo) {
  tab <- dados %>%
    filter(nivel == nivel_alvo) %>%
    distinct(ID_Publicacao, categoria_padronizada) %>%   # evita duplicar msm categoria por estudo
    count(categoria_padronizada, name = "n_estudos", sort = TRUE) %>%
    mutate(pct = round(100 * n_estudos / 50, 1))          # base = 50 estudos da amostra final
  print(tab)

  p <- ggplot(tab, aes(x = reorder(categoria_padronizada, -n_estudos), y = pct)) +
    geom_bar(stat = "identity", fill = "gray60", color = "black") +
    geom_text(aes(label = paste0(pct, "% (", n_estudos, ")")), vjust = -0.4, size = 4) +
    labs(title = titulo, x = NULL, y = "% dos estudos (N=50)") +
    ylim(0, max(tab$pct) * 1.25) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
  ggsave(file.path(output_dir, arquivo), plot = p, width = 9, height = 6, dpi = 300)
  tab
}

tab_estrategia  <- freq_nivel(classif_total, "estrategia",  "Estratégias/Desenhos de Pesquisa (Nível 1)", "grafico11a_estrategia.png")
tab_tecnica     <- freq_nivel(classif_total, "tecnica",     "Técnicas de Análise de Dados (Nível 2)",      "grafico11b_tecnica_analise.png")
tab_instrumento <- freq_nivel(classif_total, "instrumento", "Instrumentos de Coleta de Dados (Nível 3)",   "grafico11c_instrumento_coleta.png")

saveRDS(list(qual_classif=qual_classif, quant_classif=quant_classif,
             tab_estrategia=tab_estrategia, tab_tecnica=tab_tecnica, tab_instrumento=tab_instrumento),
        file.path(audit_dir, "taxonomia_gr11.rds"))

cat("\nOK - graficos salvos em", output_dir, "\n")
suppressMessages({
  library(dplyr)
  library(ggplot2)
  library(readxl)
})

df <- read_excel(data_path)
names(df) <- make.names(names(df), unique = TRUE)

df_publicacoes <- df %>%
  group_by(Ano_publicacao) %>%
  summarise(n = n(), .groups = "drop")

media_publicacoes <- mean(df_publicacoes$n, na.rm = TRUE)
ultimo_ano <- max(df_publicacoes$Ano_publicacao, na.rm = TRUE)

desastres <- data.frame(
  Ano = c(2011, 2015, 2019, 2021),
  Evento = c("Tragédias na Região Serrana do RJ",
             "Desastre em Mariana (MG)",
             "Desastre em Brumadinho (MG)",
             "Enchentes no Sul da Bahia")
)
# CORRECAO: fixar a ordem cronologica dos niveis do fator, para a legenda
# do ggplot seguir a ordem de ocorrencia dos eventos, nao a ordem alfabetica.
desastres$Evento <- factor(desastres$Evento, levels = desastres$Evento[order(desastres$Ano)])

p <- ggplot(df_publicacoes, aes(x = Ano_publicacao, y = n)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1.2) +
  geom_hline(yintercept = media_publicacoes, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_vline(data = desastres, aes(xintercept = Ano, color = Evento),
             linetype = "dashed", linewidth = 1) +
  annotate("text", x = min(df_publicacoes$Ano_publicacao) + 1,
           y = media_publicacoes + 1,
           label = paste("Média =", round(media_publicacoes, 1)),
           color = "red", size = 5, hjust = 0) +
  scale_x_continuous(
    breaks = seq(min(df_publicacoes$Ano_publicacao), ultimo_ano, by = 1),
    labels = as.integer
  ) +
  scale_color_manual(
    values = c(
      "Tragédias na Região Serrana do RJ" = "lightblue",
      "Desastre em Mariana (MG)" = "blue",
      "Desastre em Brumadinho (MG)" = "darkblue",
      "Enchentes no Sul da Bahia" = "deepskyblue"
    ),
    breaks = levels(desastres$Evento)  # <- forca a ordem cronologica na legenda
  ) +
  labs(title = "Número de Publicações por Ano", x = NULL, y = "Número de Estudos", color = "Eventos de Desastres") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "grafico5_legenda_cronologica.png"), p, width = 11, height = 7, dpi = 300)
cat("Ordem da legenda (cronologica):\n")
print(levels(desastres$Evento))
suppressMessages({
  library(dplyr)
  library(tidyr)
  library(tidytext)
  library(tm)
  library(igraph)
  library(ggraph)
  library(ggplot2)
  library(readxl)
})

df <- read_excel(data_path)
names(df) <- make.names(names(df), unique = TRUE)

clean_text <- function(text_column) {
  text <- tolower(text_column)
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("portuguese"))
  text <- stripWhitespace(text)
  return(text)
}

df$resultados <- sapply(df$resultados, clean_text)

bigramas <- df %>%
  unnest_tokens(bigram, resultados, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("palavra1", "palavra2"), sep = " ") %>%
  filter(!is.na(palavra1) & !is.na(palavra2))

palavras_irrelevantes <- c("de", "estudo", "apesar","recomenda", "da", "do", "para", "com", "por", "sobre", "entre", "a", "o", "e", "as", "os",
                           "que", "ainda","é", "um", "uma", "seu", "sua", "ser", "ter", "está", "foi", "vai", "pode", "não")

bigramas <- bigramas %>%
  filter(!palavra1 %in% palavras_irrelevantes & !palavra2 %in% palavras_irrelevantes)

bigram_freq <- bigramas %>%
  count(palavra1, palavra2, sort = TRUE) %>%
  filter(n > 3)

grafo <- graph_from_data_frame(bigram_freq)

set.seed(42)
# CORRECAO: troca de cinza claro (gray70) por escala de cor com contraste
# suficiente sobre fundo branco, mantendo a espessura proporcional a frequencia (n).
p_corrigido <- ggraph(grafo, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), color = "steelblue4", show.legend = FALSE) +
  scale_edge_width(range = c(0.4, 2)) +
  scale_edge_alpha(range = c(0.5, 1)) +
  geom_node_point(size = 5.5, color = "darkred") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4.5, color = "black") +
  labs(title = "Rede de Coocorrência de Palavras nos Resultados (cores revisadas)") +
  theme_void()

ggsave(file.path(output_dir, "grafico18_cores_revisadas.png"), p_corrigido, width = 11, height = 7, dpi = 300)
cat("N de bigramas apos filtro (n>3):", nrow(bigram_freq), "\n")
cat("OK - grafico salvo\n")
