# ========================== #
#   TESE - ANÁLISE DO CAP. 1
#   REVISÃO SISTEMÁTICA
#   Autor: Anderson Henrique
# ========================== #

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
if (length(script_arg) != 1) stop("Execute este arquivo com Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
data_path <- file.path(project_root, "dados", "banco_final.xlsx")
output_dir <- file.path(project_root, "resultados", "analise_completa")
lexicon_dir <- file.path(project_root, "dados", "auxiliares")
lexicon_path <- file.path(lexicon_dir, "OpLexicon.csv")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(lexicon_dir, recursive = TRUE, showWarnings = FALSE)
for (subdir in c("Formal", "Metodologica", "Substantivo", "Classificacao")) {
  dir.create(file.path(output_dir, subdir), recursive = TRUE, showWarnings = FALSE)
}
setwd(output_dir)



# ========================== #
#       ANÁLISE FORMAL       #
# ========================== #

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(rio)
library(skimr)  # Novo pacote para estatísticas descritivas

# 📂 Criar diretório para salvar gráficos
dir.create("Formal", showWarnings = FALSE)

# 📥 Importar dados
df <- import(data_path)


# 🔎 Verificar estrutura inicial
str(df)

# ========================== #
#   ANÁLISE VARIÁVEIS CATEGÓRICAS
# ========================== #

# 📊 Tipo de publicação (gráfico de barras com porcentagem e contagem)aa
tipo_df <- df %>%
  count(tipo) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

ggplot(tipo_df, aes(x = reorder(tipo, -percent), y = percent)) +
  geom_bar(stat = "identity", fill = "gray90", color = "black") +
  geom_text(aes(label = paste0(percent, "% (", n, ")")), vjust = -0.5, size = 5) +
  labs(
    title = "Distribuição por Tipo de Publicação",
    x = "Tipo",
    y = "Porcentagem"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  ylim(0, 100)


ggsave("Formal/tipo_publicacao.png", width = 8, height = 5)

# 📊 Tabela das fontes de publicação mais frequentes

fonte_top_df <- df %>%
  count(fonte) %>%
  mutate(percent = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n)) %>%  # Ordena do mais frequente para o menos
  head(10)  # Mantém apenas as 10 primeiras

# Exibir a tabela no console
print(fonte_top_df)

# ========================== #
#   ANÁLISE VARIÁVEIS NUMÉRICAS
# ========================== #

# 📊 Estatística descritiva usando skimr
df %>%
  select(idade, numero_autores, Ano_publicacao) %>%
  skim()


# 📊 Histogramas conjuntos de "idade" e "numero_autores"


# Calculando a média da idade
media_idade <- mean(df$idade, na.rm = TRUE)


# 📊 Histograma para "idade"
media_idade <- mean(df$idade, na.rm = TRUE)

p <- ggplot(df, aes(x = idade)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_density(aes(y = ..count..), fill = "gray90", alpha = 0.4) +
  geom_vline(aes(xintercept = media_idade), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = media_idade + 0.5, y = max(table(df$idade)) * 0.9, 
           label = paste("Média:", round(media_idade, 1)), 
           color = "red", hjust = -0.1, size = 5) +
  labs(
    title = "Distribuição da Idade dos Estudos",
    x = "Idade (anos)",
    y = "Frequência"
  ) +
  theme_minimal(base_size = 18)

p

ggsave("Formal/histograma_idade.png", width = 8, height = 5, dpi = 300)


# 📊 Histograma para "numero_autores"
media_autores <- mean(df$numero_autores, na.rm = TRUE)

p <- ggplot(df, aes(x = numero_autores)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_density(aes(y = ..count..), fill = "gray90", alpha = 0.4) +
  geom_vline(aes(xintercept = media_autores), color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = media_autores + 0.5, y = max(table(df$numero_autores)) * 0.9, 
           label = paste("Média:", round(media_autores, 1)), 
           color = "red", hjust = -0.1, size = 5) +
  labs(
    title = "Distribuição do Número de Autores por Estudo",
    x = "Número de Autores",
    y = "Frequência"
  ) +
  theme_minimal(base_size = 18)

p

ggsave("Formal/histograma_numero_autores.png", width = 8, height = 5, dpi = 300)




# 📈 Ano de Publicação ao longo do tempo 

df_publicacoes <- df %>%
  group_by(Ano_publicacao) %>%
  summarise(n = n(), .groups = "drop")  # Conta o número de publicações por ano

skim(df_publicacoes)

# 📌 Calcular a média do número de publicações
media_publicacoes <- mean(df_publicacoes$n, na.rm = TRUE)

# 📌 Determinar o último ano presente nos dados
ultimo_ano <- max(df_publicacoes$Ano_publicacao, na.rm = TRUE)

# 🔹 Criar um dataframe com os anos dos desastres e suas legendas
desastres <- data.frame(
  Ano = c(2011, 2015, 2019, 2021),
  Evento = c("Tragédias na Região Serrana do RJ",
             "Desastre em Mariana (MG)", 
             "Desastre em Brumadinho (MG)", 
             "Enchentes no Sul da Bahia")
)

# 🔹 Criar um dataframe para a legenda manual das linhas
legenda_desastres <- data.frame(
  Ano = desastres$Ano,
  Evento = desastres$Evento,
  Tipo = "Evento de Desastre"
)



# Gráfico atualizado com suavização e anotações dos eventos
library(ggplot2)
library(dplyr)

p <- ggplot(df_publicacoes, aes(x = Ano_publicacao, y = n)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 1.2) +
  
  # Linha da média
  geom_hline(yintercept = media_publicacoes, linetype = "dashed", color = "red", linewidth = 0.8) +
  
  # Linhas verticais dos eventos, sem rótulos
  geom_vline(data = desastres, aes(xintercept = Ano, color = Evento), 
             linetype = "dashed", linewidth = 1) +
  
  # Anotação da média
  geom_text(aes(x = min(Ano_publicacao) + 1, y = media_publicacoes + 1, 
                label = paste("Média =", round(media_publicacoes, 1))), 
            color = "red", size = 5, hjust = 0) +
  
  scale_x_continuous(
    breaks = seq(min(df_publicacoes$Ano_publicacao), ultimo_ano, by = 1),
    labels = as.integer
  ) +
  scale_color_manual(values = c(
    "Tragédias na Região Serrana do RJ" = "lightblue",
    "Desastre em Mariana (MG)" = "blue",
    "Desastre em Brumadinho (MG)" = "darkblue",
    "Enchentes no Sul da Bahia" = "deepskyblue"
  )) +
  labs(
    title = "Número de Publicações por Ano",
    x = NULL,
    y = "Número de Estudos",
    color = "Eventos de Desastres"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

# Exibir e salvar
print(p)

# Ajuste da margem inferior no tema
p <- p + theme(
  plot.margin = margin(t = 15, r = 15, b = 30, l = 15)  # aumenta espaço inferior
)

# Salvar com altura maior e limites desativados
ggsave(
  filename = "Formal/publicacoes_por_ano_suave_linhas_sem_texto.png",
  plot = p,
  width = 12,
  height = 8,              # altura maior
  units = "in",
  dpi = 300,
  limitsize = FALSE        # evita corte
)


# Carregar pacotes
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# 📌 Criar um corpus com os títulos dos trabalhos
corpus <- Corpus(VectorSource(df$titulo))

# 📌 Pré-processamento do texto
corpus <- tm_map(corpus, content_transformer(tolower))  # Converter para minúsculas
corpus <- tm_map(corpus, removePunctuation)  # Remover pontuação
corpus <- tm_map(corpus, removeNumbers)  # Remover números
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))  # Remover palavras comuns
corpus <- tm_map(corpus, stripWhitespace)  # Remover espaços extras

# 📊 Criar matriz de termos (TF)
dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)  # Contar frequência das palavras
df_words <- data.frame(word = names(word_freqs), freq = word_freqs)  # Criar dataframe

# 🎨 Gerar a nuvem de palavras
set.seed(123)  # Para reprodutibilidade
w <- wordcloud(words = df_words$word, freq = df_words$freq, min.freq = 2,  # Define frequência mínima
          max.words = 100, random.order = FALSE, rot.per = 0.3, 
          colors = brewer.pal(8, "Dark2"))  # Cores vibrantes


# ========================== #
#      FIM DO BLOCO FORMAL    #
# ========================== #



# ========================== #
#     ANÁLISE METODOLÓGICA   #
# ========================== #

#  Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(rio)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tidyr)  # Para separar os valores corretamente
library(tidytext)
library(igraph)  # Para criar a rede de palavras
library(ggraph)  # Para visualizar a rede


# Criar diretório para salvar gráficos
dir.create("Metodologica", showWarnings = FALSE)

rm(df)
df <- import(data_path)

# Renomear colunas para remover espaços extras e quebras de linha
colnames(df) <- trimws(gsub("\\n", "", colnames(df)))  # Remove quebras de linha e espaços extras

# Substituir valores numéricos por descrições completas
df$perg_pesquisa <- factor(df$perg_pesquisa, 
                           levels = c(0, 1),
                           labels = c("Com Pergunta de Pesquisa", 
                                      "Sem Pergunta de Pesquisa"))

#  Criar gráfico de barras para `perg_pesquisa`
library(stringr)

df %>%
  count(perg_pesquisa) %>%
  mutate(
    percent = round(n / sum(n) * 100, 1),
    perg_pesquisa_wrap = str_wrap(perg_pesquisa, width = 30)  # quebra de linha para labels longos
  ) %>%
  ggplot(aes(x = reorder(perg_pesquisa_wrap, -percent), y = percent)) +
  geom_bar(stat = "identity", fill = "gray90", color = "black") +
  geom_text(aes(label = paste0(percent, "% (", n, ")")), vjust = -0.5, size = 5) +
  labs(
    title = "Distribuição da Pergunta de Pesquisa",
    x = NULL,
    y = "Porcentagem"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(t = 15, r = 20, b = 30, l = 15)
  ) +
  ylim(0, 100)




#  Renomear colunas para remover espaços extras e quebras de linha
colnames(df) <- trimws(gsub("\\n", "", colnames(df)))  # Remove quebras de linha e espaços extras

#  Substituir valores numéricos por descrições completas

df$hipotese <- factor(df$hipotese, levels = c(0, 1),
                      labels = c("Sem Hipótese", "Com Hipótese"))

df$metodologia <- factor(df$metodologia, levels = c(0, 1),
                         labels = c("Sem Metodologia", "Com Metodologia"))

df$efeito <- factor(df$efeito, levels = c(0, 1),
                    labels = c("Sem Efeito Causal", "Com Efeito Causal"))

df$indicador <- factor(df$indicador, levels = c(0, 1),
                       labels = c("Sem Indicador", "Com Indicador"))

df$tipo_indicador <- factor(df$tipo_indicador, levels = c(1, 2, 3, 4, 5, 6),
                            labels = c("Indicador-Insumo", "Indicador-Processo", 
                                       "Indicador-Produto", "Indicador-Resultado",
                                       "Indicador-Impacto", ">1 Indicador"))

df$abordagem_metodologica <- factor(df$abordagem_metodologica, levels = c(1, 2, 3),
                                    labels = c("Qualitativa", "Quantitativa", "Mista"))

df$d_pesq <- factor(df$d_pesq, levels = c(1, 2, 3),
                    labels = c("Experimental", "Quase-experimental", "Observacional"))

df$fonte_dados <- factor(df$fonte_dados, levels = c(1, 2, 3),
                         labels = c("Primária", "Secundária", "Ambas"))

df$dados_abertos <- factor(df$dados_abertos, levels = c(0, 1),
                           labels = c("Sem Dados Abertos", "Dados Abertos"))


df$Caso <- factor(df$Caso, levels = c(1, 2, 3, 4, 5),
                           labels = c("Local", "Estadual", "Regional",
                                      "Nacional", "NA"))



# 📊 Função para gerar gráficos de barras, garantindo que as labels não se sobreponham
plot_bar <- function(var, var_name, file_name, use_legend = FALSE) {
  library(ggplot2)
  library(dplyr)
  library(stringr)
  
  # Pré-processamento
  plot_data <- df %>%
    count(!!sym(var)) %>%
    mutate(
      percent = round(n / sum(n) * 100, 1),
      label_wrap = str_wrap(!!sym(var), width = 30)
    )
  
  # Gráfico
  p <- ggplot(plot_data, aes(x = reorder(label_wrap, -percent), y = percent, fill = !!sym(var))) +
    geom_bar(stat = "identity", fill = "gray90", color = "black") +
    geom_text(aes(label = paste0(percent, "% (", n, ")")), 
              vjust = ifelse(use_legend, 0.5, -0.5), size = 5) +
    scale_y_continuous(
      limits = c(0, NA),  # limite inferior em 0, superior automático
      expand = expansion(mult = c(0, 0.1))  # expande o topo para o texto caber
    ) +
    labs(
      title = paste("Distribuição de", var_name),
      x = NULL,
      y = "Porcentagem"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(t = 15, r = 20, b = 30, l = 15)
    )
  
  # Controle de legenda
  if (use_legend) {
    p <- p + theme(legend.position = "right") +
      guides(fill = guide_legend(title = var_name))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # Salvar
  ggsave(
    filename = paste0("Metodologica/", file_name, ".jpeg"),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300,
    units = "in",
    limitsize = FALSE
  )
}
  
 
# 📊 Criar gráfico ajustado para `tipo_indicador`, removendo `NA`

p <- df %>%
  filter(!is.na(tipo_indicador) & tipo_indicador != "NA") %>%
  count(tipo_indicador) %>%
  mutate(
    percent = round(n / sum(n) * 100, 1),
    tipo_indicador_wrap = str_wrap(tipo_indicador, width = 30)
  ) %>%
  ggplot(aes(x = reorder(tipo_indicador_wrap, -percent), y = percent)) +
  geom_bar(stat = "identity", fill = "gray90", color = "black") +
  geom_text(aes(label = paste0(percent, "% (", n, ")")),
            vjust = -0.3, size = 5) +
  labs(
    title = "Distribuição do Tipo de Indicador",
    x = NULL,
    y = "Porcentagem"
  ) +
  ylim(0, 100) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.position = "none",
    plot.margin = margin(t = 15, r = 20, b = 30, l = 15)
  )

# Exibir
print(p)

# Salvar com tamanho adequado
ggsave(
  "Formal/tipo_indicador.jpeg",
  plot = p,
  width = 13,
  height = 8,
  dpi = 300,
  units = "in",
  limitsize = FALSE
)




# **LOTE 1**
plot_bar("hipotese", "Hipótese", "hipotese")
plot_bar("metodologia", "Metodologia", "metodologia")
plot_bar("efeito", "Efeito Causal", "efeito_causal")
plot_bar("Caso", "Caso", "caso")

# **LOTE 2**
plot_bar("indicador", "Utilização de Indicador", "uso_indicador")
ggsave("Metodologica/tipo_indicador.jpeg", plot = p, width = 8, height = 5, dpi = 300)
plot_bar("abordagem_metodologica", "Abordagem Metodológica", "abordagem_metodologica")

# **LOTE 3**
plot_bar("d_pesq", "Desenho de Pesquisa", "desenho_pesquisa")
plot_bar("fonte_dados", "Fonte dos Dados", "fonte_dados")
plot_bar("dados_abertos", "Dados Abertos", "dados_abertos")
ggsave("Metodologica/pergunta_pesquisa.jpeg", width = 8, height = 5)



# ========================== #
# SEPARANDO AS TÉCNICAS 
# ========================== #

#  Função para dividir corretamente as técnicas e garantir separação real
split_and_clean <- function(column) {
  df_long <- df %>%
    select(all_of(column)) %>%
    filter(!is.na(!!sym(column))) %>%  # Remove valores NA
    mutate(!!sym(column) := tolower(!!sym(column))) %>%  # Converte para minúsculas
    separate_rows(!!sym(column), sep = ",") %>%  # Separa por vírgula corretamente
    mutate(!!sym(column) := trimws(!!sym(column))) %>%  # Remove espaços extras
    filter(!!sym(column) != "" & !!sym(column) != "na") %>%  # Remove "na" e valores vazios
    mutate(!!sym(column) := str_replace_all(!!sym(column), 
                                            c("estatistica descritiva" = "estatística descritiva",
                                              "análise documenta" = "análise documental",
                                              "análise documentall" = "análise documental",
                                              "análise de discurso" = "análise do discurso",
                                              "revisão bibliográfica" = "revisão da literatura",
                                              "revisão de literatura " = "revisão da literatura",
                                              "revisão de literatura" = "revisão da literatura",
                                              "revisão sistemática" = "revisão da literatura",
                                              "revisão de literatura e análise documental" = "revisão da literatura",
                                              "revisão da literaturae análise documental" = "revisão da literatura",
                                              "revisão de literatura " = "revisão da literatura",
                                              "estudo de caso e análise documental" = "estudo de caso",
                                              "entrevistas semiestruturada" = "entrevistas semiestruturadas",
                                              "entrevistas semiestruturadass" = "entrevistas semiestruturadas",
                                              "análise discursiva" = "análise do discurso")))  # 🔹 Corrigindo nomes
  
  return(df_long)
}
#  Aplicar a função para quantitativa e qualitativa
quant_df <- split_and_clean("quantitativa")
qual_df <- split_and_clean("qualitativa")

# Criar tabelas de frequência ajustadas com porcentagem, excluindo "na"
quant_freq <- quant_df %>%
  count(quantitativa, sort = TRUE) %>%
  rename(Tecnica = quantitativa, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1))# %>%
 # head(10)  # Top 10 técnicas quantitativas

qual_freq <- qual_df %>%
  count(qualitativa, sort = TRUE) %>%
  rename(Tecnica = qualitativa, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1)) %>%
  head(7)  # Top 7 técnicas qualitativas


skim(qual_freq)


# Exibir as tabelas no console
print("Top 10 Técnicas de Análise Quantitativa")
print(quant_freq)

print("Top 10 Técnicas de Análise Qualitativa")
print(qual_freq)



# 📌 Converter os nomes das técnicas para iniciar com maiúscula
library(stringr)
quant_freq$Tecnica <- str_to_title(quant_freq$Tecnica)  # Formata maiúscula inicial
qual_freq$Tecnica <- str_to_title(qual_freq$Tecnica)  # Formata maiúscula inicial

# 📊 Gráfico para Técnicas Quantitativas
ggplot(quant_freq, aes(x = reorder(Tecnica, -Quantidade), y = Porcentagem)) +
  geom_bar(stat = "identity", fill = "gray60", color = "black") +  # Cor cinza com borda preta
  geom_text(aes(label = paste0(Porcentagem, "% (", Quantidade, ")")), 
            vjust = -0.5, size = 5) +  # Adiciona % e quantidade no topo
  labs(title = "Top 10 Técnicas de Análise Quantitativa",
       x = "Técnica",
       y = "Porcentagem") +
  ylim(0, 100) +  # Eixo Y de 0 a 100
  theme_minimal(base_size = 12) +  # 🔹 Define base do tema com tamanho 12
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # 🔹 Labels do eixo X tamanho 12
    axis.text.y = element_text(size = 12),  # 🔹 Labels do eixo Y tamanho 12
    axis.title.x = element_text(size = 12, face = "bold"),  # 🔹 Rótulo eixo X negrito
    axis.title.y = element_text(size = 12, face = "bold"),  # 🔹 Rótulo eixo Y negrito
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # 🔹 Título centralizado e maior
  )

# 📊 Gráfico para Técnicas Qualitativas
ggplot(qual_freq, aes(x = reorder(Tecnica, -Quantidade), y = Porcentagem)) +
  geom_bar(stat = "identity", fill = "gray60", color = "black") +  # Cor cinza com borda preta
  geom_text(aes(label = paste0(Porcentagem, "% (", Quantidade, ")")), 
            vjust = -0.5, size = 5) +  # Adiciona % e quantidade no topo
  labs(title = "Top 10 Técnicas de Análise Qualitativa",
       x = "Técnica",
       y = "Porcentagem") +
  ylim(0, 100) +  # Eixo Y de 0 a 100
  theme_minimal(base_size = 12) +  # 🔹 Define base do tema com tamanho 12
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # 🔹 Labels do eixo X tamanho 12
    axis.text.y = element_text(size = 12),  # 🔹 Labels do eixo Y tamanho 12
    axis.title.x = element_text(size = 12, face = "bold"),  # 🔹 Rótulo eixo X negrito
    axis.title.y = element_text(size = 12, face = "bold"),  # 🔹 Rótulo eixo Y negrito
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)  # 🔹 Título centralizado e maior
  )





# Contagem da palavra "na" nas variáveis quantitativa e qualitativa
na_str_quant <- sum(tolower(df$quantitativa) == "na", na.rm = TRUE)
na_str_qual <- sum(tolower(df$qualitativa) == "na", na.rm = TRUE)

#  Calcular porcentagem da palavra "na"
total_linhas <- nrow(df)
perc_na_str_quant <- round((na_str_quant / total_linhas) * 100, 1)
perc_na_str_qual <- round((na_str_qual / total_linhas) * 100, 1)

# Exibir os resultados
print(paste("Ocorrências da palavra 'na' em quantitativa:", na_str_quant, "(", perc_na_str_quant, "%)"))
print(paste("Ocorrências da palavra 'na' em qualitativa:", na_str_qual, "(", perc_na_str_qual, "%)"))


# ========================== #
#  temp_inicio E temp_final
# ========================== #
df$temp_inicio_novo <- as.numeric(df$temp_inicio)
df$temp_final_novo <- as.numeric(df$temp_final)

# ========================== #
# CÁLCULO DO TEMPO DE ESTUDO 
# ========================== #

# Criar nova coluna para a duração do estudo
df_1 <- df %>%
  mutate(duracao_estudo = temp_final_novo - temp_inicio_novo) %>%
  filter(!is.na(duracao_estudo) & duracao_estudo >= 0)  # Remove valores inconsistentes

# 📊 Calcular estatísticas da duração dos estudos
media_duracao <- round(mean(df_1$duracao_estudo, na.rm = TRUE), 1)
mediana_duracao <- round(median(df_1$duracao_estudo, na.rm = TRUE), 1)
min_duracao <- min(df_1$duracao_estudo, na.rm = TRUE)
max_duracao <- max(df_1$duracao_estudo, na.rm = TRUE)

# 📋 Contagem de valores ausentes (NA) em temp_inicio
na_inicio <- sum(is.na(df$temp_inicio_novo))

# 📊 Calcular porcentagem de valores ausentes em relação ao total de estudos
total_linhas <- nrow(df)
perc_na_inicio <- round((na_inicio / total_linhas) * 100, 1)

# 📢 Exibir os resultados no console
print(paste("Duração média dos estudos:", media_duracao, "anos"))
print(paste("Mediana da duração dos estudos:", mediana_duracao, "anos"))
print(paste("Menor duração registrada:", min_duracao, "anos"))
print(paste("Estudos sem data de coleta informada (NA):", na_inicio, "(", perc_na_inicio, "%)"))
print(paste("Maior duração registrada:", max_duracao, "anos"))


rm(df_autores,df_long)

## Analise de Cluster ##


# Resolver problema de colunas duplicadas antes
# 📌 Verificar colunas duplicadas no dataframe
duplicadas <- names(df)[duplicated(names(df))]

print(duplicadas)  # Exibe quais colunas estão duplicadas
# 📌 Criar um novo conjunto de nomes, renomeando as duplicatas
novo_nomes <- make.names(names(df), unique = TRUE)

# 📌 Aplicar os novos nomes ao dataframe
colnames(df) <- novo_nomes
# 📌 Exibir os novos nomes das colunas para conferência
print(colnames(df))



# 📚 Carregar pacotes necessários
library(dplyr)
library(tidyr)
library(igraph)
library(ggraph)
library(ggplot2)

# 1️⃣ Criar um ID único para cada publicação (caso ainda não exista)
df <- df %>%
  mutate(ID_Publicacao = row_number())

# 2️⃣ Substituir " e " por "," APENAS na variável "autor" para padronizar a separação
df <- df %>%
  mutate(autor = gsub(" e ", ",", autor))  

# 3️⃣ **Separar apenas a variável "autor" corretamente**
df_autores <- df %>%
  select(ID_Publicacao, autor) %>%  # Mantém apenas as colunas essenciais para o processo
  separate(autor, into = paste0("autor", 1:7), sep = ",", fill = "right", extra = "drop")

# 4️⃣ **Rejuntar com o dataframe original** (para manter todas as outras colunas intactas)
df_autores <- left_join(df, df_autores, by = "ID_Publicacao")

# 5️⃣ **Converter para formato longo, mantendo todas as colunas relevantes**
df_long <- df_autores %>%
  pivot_longer(cols = starts_with("autor"), names_to = "autor_num", values_to = "autor") %>%
  filter(!is.na(autor) & autor != "")  # Remover valores vazios

# 6️⃣ Criar os pares de coautoria dentro de cada publicação
coautor_edges <- df_long %>%
  group_by(ID_Publicacao) %>%
  summarise(
    pares = list(
      if (n() > 1) {  
        combn(autor, 2, FUN = function(x) paste(x, collapse = " - "))
      } else {
        NA_character_  
      }
    ),
    .groups = "drop"
  ) %>%
  unnest(pares) %>%
  filter(!is.na(pares)) %>%
  separate(pares, into = c("autor1", "autor2"), sep = " - ")

# 7️⃣ Criar o grafo de coautoria
grafo_coautoria <- graph_from_data_frame(coautor_edges, directed = FALSE)

# 8️⃣ Aplicar o Algoritmo de Louvain para identificar clusters
clusters <- cluster_louvain(grafo_coautoria, resolution = 0.4)
df_clusters <- data.frame(
  autor = V(grafo_coautoria)$name,
  cluster = as.factor(membership(clusters))
)

# 9️⃣ Reduzir o número de clusters caso haja muitos grupos pequenos
num_clusters <- length(unique(df_clusters$cluster))
if (num_clusters > 5) {
  df_clusters$cluster <- as.factor(cut(as.numeric(df_clusters$cluster), breaks = 5, labels = 1:5))
}
V(grafo_coautoria)$cluster <- df_clusters$cluster

# 🔹 Criar uma lista apenas com os nomes que aparecem como "autor1"
autores_principais <- unique(df_autores$autor1)

# 🔹 Criar o gráfico de clusters, exibindo apenas os nomes dos autores principais
ggraph(grafo_coautoria, layout = "kk") +  
  geom_edge_link(alpha = 0.3, color = "gray") +  
  geom_node_point(aes(color = cluster), size = 5) +  
  geom_node_text(aes(label = ifelse(name %in% autores_principais, name, NA)), 
                 repel = TRUE, size = 5, color = "black") +  
  scale_color_manual(values = rainbow(length(unique(df_clusters$cluster)))) +  
  theme_minimal() +
  labs(title = "Clusters de Coautoria na Rede de Pesquisadores",
       color = "Cluster")





#========================================
#
# Bloco Substantiva 

#========================================

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(rio)
library(tidytext)  # Para análise de sentimentos
library(tm)  # Para limpeza do texto
library(topicmodels)  # Para modelagem de tópicos (LDA)
library(ggplot2)
library(tidytext)
library(stringr)
library(tidyr)  # Para separar os valores corretamente



# 🔄 Substituir valores numéricos por descrições completas
df$modelo_gestao <- factor(df$modelo_gestao, levels = c(1, 2, 3, 4),
                           labels = c("Modelo Cíclico", 
                                      "Modelo de Emergência",
                                      "Modelo de Governança\nMultissetorial", 
                                      "Modelo Prospectivo,\nCorretivo e Compensatório"))

df$etapa <- factor(df$etapa, levels = c(1, 2, 3, 4),
                   labels = c("Pré-aguda", "Crise-aguda", "Pós-crise", ">1"))

df$def_gestao_risco <- factor(df$def_gestao_risco, levels = c(0, 1),
                              labels = c("Sem Definição", "Com Definição"))


#  Função para gerar gráficos de barras formatados corretamente
plot_bar <- function(var, var_name, file_name, use_legend = FALSE) {
  library(ggplot2)
  library(dplyr)
  library(stringr)
  
  # Pré-processamento
  plot_data <- df %>%
    count(!!sym(var)) %>%
    mutate(
      percent = round(n / sum(n) * 100, 1),
      label_wrap = str_wrap(!!sym(var), width = 30)
    )
  
  # Criação do gráfico
  p <- ggplot(plot_data, aes(x = reorder(label_wrap, -percent), y = percent)) +
    geom_bar(stat = "identity", fill = "gray90", color = "black") +
    geom_text(aes(label = paste0(percent, "% (", n, ")")),
              vjust = ifelse(use_legend, 0.5, -0.5), size = 5) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = paste("Distribuição de", var_name),
      x = NULL,
      y = "Porcentagem"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(t = 15, r = 20, b = 30, l = 15)
    )
  
  # Legenda opcional
  if (use_legend) {
    p <- p + theme(legend.position = "right") +
      guides(fill = guide_legend(title = var_name))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # Exibir (opcional)
  print(p)
  
  # Salvar o gráfico
  ggsave(
    filename = paste0("Substantivo/", file_name, ".jpeg"),
    plot = p,
    width = 12,
    height = 8,
    dpi = 300,
    units = "in",
    limitsize = FALSE
  )
}

# ========================== #
#  Geração de gráficos
# ========================== #
plot_bar("modelo_gestao", "Modelo de Gestão de Risco", "modelo_gestao")
plot_bar("etapa", "Etapa da Gestão de Risco", "etapa")
plot_bar("def_gestao_risco", "Definição de Gestão de Risco e Desastre", "def_gestao_risco")

# ========================== #
#  SEPARANDO OS TIPOS DE DESASTRE
# ========================== #

# 🔄 Função para dividir corretamente os tipos de desastre e garantir separação real

# 🔄 Função para dividir corretamente os tipos de desastre e garantir separação real
split_and_clean <- function(column) {
  df_long <- df %>%
    select(all_of(column)) %>%
    filter(!is.na(!!sym(column))) %>%  # Remove valores NA
    mutate(!!sym(column) := tolower(!!sym(column))) %>%  # Converte para minúsculas
    separate_rows(!!sym(column), sep = ",") %>%  # Separa por vírgula corretamente
    mutate(!!sym(column) := trimws(!!sym(column))) %>%  # Remove espaços extras
    filter(!!sym(column) != "" & !!sym(column) != "na") %>%  # Remove "na" e valores vazios
    mutate(!!sym(column) := str_replace_all(!!sym(column), c(
      "vendavais" = "vendaval",
      "incêndios" = "incêndios florestais",
      "incêndios florestais florestais" = "incêndios florestais", 
      "rompimento de barragem de rejeitos de mineração" = "rompimento de barragem",
      "Rompimento/ colapso de barragens de mineração" = "rompimento de barragem",
      "rompimento de barragens" = "rompimento de barragem",
      "rompimento de barragem de mineração" = "rompimento de barragem",
      "estiagens" = "estiagem",
      "erosões" = "erosão",
      "enxurradas" = "enxurradas", 
      "impactos ambientais" = "desastres naturais",
      "gestão de riscos ambientais" = "desastres naturais",
      "gestão integral de riscos de desastres naturais" = "desastres naturais",
      "gestão de desastres" = "desastres naturais",
      "deslizamentos" = "deslizamento",
      "desastres naturais e tecnológicos" = "desastres tecnológicos",
      "quedas de barreiras" = "deslizamento",
      "deslizamento de terra" = "deslizamento",
      "escorregamentos/deslizamento" = "deslizamento",
      "eventos meteorológicos extremos" = "enxurradas",
      "chuvas intensas" = "enxurradas",
      "queda de barreiras" = "deslizamento",
      "terremotos" = "tremor de terra",
      "terremoto" = "tremor de terra",
      "derramamento de petróleo" = "derramamento de produtos químico",
      "riscos derivados do uso da ciência e tecnologia" = "desastres tecnológicos",
      "ocupação de áreas suscetíveis a deslizamento" = "deslizamento"
    ))) %>%
    mutate(!!sym(column) := recode(!!sym(column),
      "desastres naturais" = "desastres",
      "deslizamentos" = "deslizamento",
      "deslizamento de terra" = "deslizamento",
      "rompimento de barragem de mineração" = "rompimento de barragem",
      "incêndios florestais florestais" = "incêndios florestais",
      .default = !!sym(column)
    ))  # Segunda passagem para resíduos criados pelo próprio mapeamento
  
  return(df_long)
}
#  Aplicar a função para tipo_desastre
desastre_df <- split_and_clean("tipo_desastre")

write.csv(desastre_df, "desastre_df.csv")

# Criar tabela de frequência ajustada com porcentagem
desastre_freq <- desastre_df %>%
  count(tipo_desastre, sort = TRUE) %>%
  rename(Tipo_Desastre = tipo_desastre, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1)) %>%
  head(10)  # Top 10 Tipos de Desastre




#  Ajustar os labels para começarem com letra maiúscula
library(stringr)
desastre_freq$Tipo_Desastre <- str_to_title(desastre_freq$Tipo_Desastre)

#  Exibir a tabela no console
print("Top 10 Tipos de Desastre")
print(desastre_freq)

#  Contagem da palavra "na" na variável tipo_desastre
na_str_desastre <- sum(tolower(df$tipo_desastre) == "na", na.rm = TRUE)

# Calcular porcentagem da palavra "na"
total_linhas <- nrow(df)
perc_na_str_desastre <- round((na_str_desastre / total_linhas) * 100, 1)

# Exibir os resultados
print(paste("Ocorrências da palavra 'na' em tipo_desastre:", na_str_desastre, "(", perc_na_str_desastre, "%)"))

# ========================== #
#  GRÁFICO DE BARRAS PARA TIPO DE DESASTRE (TOP 10)
# ========================== #

#  Criar gráfico ajustado para `tipo_desastre`
library(ggplot2)

p <- ggplot(desastre_freq, aes(x = reorder(Tipo_Desastre, -Porcentagem), y = Porcentagem)) +
  geom_bar(stat = "identity", fill = "gray90", color = "black") +
  geom_text(aes(label = paste0(Porcentagem, "% (", Quantidade, ")")), 
            vjust = -0.3, size = 5) +
  scale_y_continuous(
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Top 10 Tipos de Desastre",
    x = NULL,
    y = "Porcentagem"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, lineheight = 1.2),
    legend.position = "none",
    plot.margin = margin(t = 15, r = 20, b = 30, l = 15)
  )

# Visualizar
print(p)

# Salvar
ggsave("Substantivo/top10_tipos_desastre.jpeg", plot = p, width = 12, height = 8, dpi = 300, units = "in", limitsize = FALSE)


# ========================== #
# SEPARANDO OS AUTORES DE GESTÃO DE DESASTRE
# ========================== #

#  Renomear colunas para remover espaços extras e quebras de linha
colnames(df) <- trimws(gsub("\\n", "", colnames(df)))

# O Excel/R pode normalizar os parênteses e a vírgula deste cabeçalho de
# maneiras diferentes. Usar um nome interno estável torna o script portátil.
col_autores <- grep("^prinp_autores_gestao_desastre", colnames(df), value = TRUE)
if (length(col_autores) != 1) {
  stop("Não foi possível identificar univocamente a coluna de autores citados.")
}
colnames(df)[colnames(df) == col_autores] <- "autores_gestao_desastre"

# Função para dividir corretamente os autores e garantir separação real
# Aplicar a função para autores_gestao_desastre
autores_df <- split_and_clean("autores_gestao_desastre")

# Criar tabela de frequência ajustada com porcentagem
autores_freq <- autores_df %>%
  count(autores_gestao_desastre, sort = TRUE) %>%
  rename(Autor = autores_gestao_desastre, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1))  %>%
  head(10)  # Top 10 Tipos de Desastre

# 🔹 Exibir a tabela no console
print("Top Autores Mais Citados em Gestão de Desastre")
print(autores_freq)



# 📌 Renomear colunas para remover espaços extras e quebras de linha
colnames(df) <- trimws(gsub("\\n", "", colnames(df)))

# 📌 Função para dividir corretamente os autores e garantir separação real
split_and_clean <- function(column) {
  df_long <- df %>%
    select(all_of(column)) %>%
    filter(!is.na(!!sym(column))) %>%  # Remove valores NA
    separate_rows(!!sym(column), sep = ",") %>%  # Separa por vírgula corretamente
    mutate(!!sym(column) := trimws(!!sym(column))) %>%  # Remove espaços extras
    filter(!!sym(column) != "" & !!sym(column) != "na")  # Remove "na" e valores vazios
  
  return(df_long)
}

# 📌 Aplicar a função para autores_gestao_desastre
autores_df <- split_and_clean("autores_gestao_desastre")

# 📊 Criar tabela de frequência ajustada com porcentagem
autores_freq <- autores_df %>%
  count(autores_gestao_desastre, sort = TRUE) %>%
  rename(Autor = autores_gestao_desastre, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1))  %>%
  head(10)  

# 📌 Ajustar os labels para começarem com letra maiúscula
autores_freq$Autor <- str_to_title(autores_freq$Autor)

# 📌 Criar o gráfico de barra descendente
ggplot(autores_freq, aes(x = reorder(Autor, -Quantidade), y = Porcentagem, fill = Autor)) +
  geom_bar(stat = "identity", color = "black", fill = "gray60") +  # 🔹 Cor cinza
  geom_text(aes(label = paste0(Porcentagem, "% (", Quantidade, ")")), 
            vjust = -0.3, size = 5) +  # 🔹 Adiciona texto no topo das barras
  labs(title = "Autores Mais Citados em Gestão de Desastre",
       x = "Autor",
       y = "Porcentagem") +
  ylim(0, 20) +  # 🔹 Garante eixo Y de 0 a 100
  theme_minimal(base_size = 12) +  # 🔹 Define base do tema com tamanho 12
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # 🔹 Labels eixo X tamanho 12
    axis.text.y = element_text(size = 12),  # 🔹 Labels eixo Y tamanho 12
    axis.title.x = element_text(size = 12, face = "bold"),  # 🔹 Rótulo eixo X tamanho 12 e negrito
    axis.title.y = element_text(size = 12, face = "bold"),  # 🔹 Rótulo eixo Y tamanho 12 e negrito
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # 🔹 Título centralizado e maior
    legend.position = "none"  # 🔹 Remove legenda
  )












# ========================== #
#  RECODIFICAÇÃO DA VARIÁVEL `dominio_tema`
# ========================== #
df <- df[, !duplicated(names(df))] #Remover colunas duplicadas 

anyDuplicated(names(df))  #Checar se foram removidas

df$dominio_tema <- ifelse(df$dominio_tema == "Administração Pública", "Políticas Públicas", df$dominio_tema)

# ========================== #
# 📋 CONTAGEM E PORCENTAGEM DOS DOMÍNIOS TEMÁTICOS
# ========================== #

# Criar tabela de frequência ajustada com porcentagem
dominio_freq <- df %>%
  count(dominio_tema, sort = TRUE) %>%
  rename(Dominio_Tema = dominio_tema, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1)) %>%
  head(10)  # Top 10 Domínios Temáticos

# 🔄 Ajustar os labels para começarem com letra maiúscula
dominio_freq$Dominio_Tema <- str_to_title(dominio_freq$Dominio_Tema)

# 🔹 Exibir a tabela no console
print("Top 10 Domínios Temáticos (Com Recodificação)")
print(dominio_freq)

# ========================== #
# 📊 GRÁFICO DE BARRAS (TOP 10 DOMÍNIOS TEMÁTICOS)
# ========================== #

# 🔄 Criar gráfico ajustado para `dominio_tema`
p <- ggplot(dominio_freq, aes(x = reorder(Dominio_Tema, -Porcentagem), y = Porcentagem, fill = Dominio_Tema)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = paste0(Porcentagem, "% (", Quantidade, ")")), 
            vjust = -0.3, size = 4) +  # 🔄 Ajusta os valores para cima das barras
  scale_fill_grey(start = 0.3, end = 0.9) +
  labs(title = "Top 10 Domínios Temáticos",
       x = "Domínio Temático",
       y = "Porcentagem") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, lineheight = 1.2),  # 🔄 Rótulos escalonados
        legend.position = "none") +
  ylim(0, 50)

p

# 📂 Salvar o gráfico em `.jpeg`
ggsave("Substantivo/dominio_tema.jpeg", plot = p, width = 8, height = 5, dpi = 300)


# ========================== #
#  LIMPEZA E PROCESSAMENTO DO TEXTO
# ========================== #

# Função para processar o texto e remover palavras irrelevantes
process_text <- function(text_column) {
  text <- tolower(paste(text_column, collapse = " "))  # Junta todo o texto
  text <- removePunctuation(text)  # Remove pontuação
  text <- removeNumbers(text)  # Remove números
  text <- removeWords(text, stopwords("portuguese"))  # Remove palavras comuns
  text <- stripWhitespace(text)  # Remove espaços extras
  return(text)
}

# Criar corpus de texto processado
resultados_text <- process_text(df$resultados)
resultados_corpus <- Corpus(VectorSource(resultados_text))
resultados_tdm <- TermDocumentMatrix(resultados_corpus)
resultados_matrix <- as.matrix(resultados_tdm)
resultados_word_freq <- sort(rowSums(resultados_matrix), decreasing = TRUE)

# Criar tabela de frequência com as 20 palavras mais comuns
resultados_freq <- data.frame(Termo = names(resultados_word_freq), Frequencia = resultados_word_freq)
resultados_freq <- resultados_freq %>% arrange(desc(Frequencia)) %>% head(20)  # Top 20 palavras mais usadas

# 🔹 Exibir a tabela no console
print("Top 20 Termos Mais Usados nos Resultados")
print(resultados_freq)

# ========================== #
#  NUVEM DE PALAVRAS
# ========================== #

# ️ Criar nuvem de palavras para os resultados 30 palavras

jpeg("Substantivo/nuvem_resultados.jpeg", width = 800, height = 600, res = 150)
wordcloud(resultados_freq$Termo, resultados_freq$Frequencia, scale = c(4, 0.5), 
          colors = brewer.pal(8, "Dark2"), random.order = FALSE)
dev.off()



# ========================== #
#  Análise de Sentimentos
# ========================== #

#  Função para limpar e preparar o texto para análise de sentimentos
clean_text <- function(text_column) {
  text <- tolower(text_column)  # Converter para minúsculas
  text <- removePunctuation(text)  # Remover pontuação
  text <- removeNumbers(text)  # Remover números
  text <- removeWords(text, stopwords("portuguese"))  # Remover stopwords
  text <- stripWhitespace(text)  # Remover espaços extras
  return(text)
}

# Aplicar a função na variável `resultados`
df$resultados <- sapply(df$resultados, clean_text)

# ========================== #
# 📋 BAIXANDO LÉXICO DE SENTIMENTOS EM PORTUGUÊS
# ========================== #
# 🔄 Carregar OpLexicon atualizado
if (!file.exists(lexicon_path)) {
  download.file(
    "https://raw.githubusercontent.com/marlovss/OpLexicon/main/OpLexicon.csv",
    lexicon_path,
    mode = "wb",
    quiet = FALSE
  )
}
oplexicon <- read.delim(lexicon_path, header = FALSE, stringsAsFactors = FALSE)

# Inspecionar as primeiras linhas do OpLexicon
head(oplexicon)


# 🔄 Separar as colunas corretamente
oplexicon <- oplexicon %>%
  separate(V1, into = c("word", "category", "sentiment"), sep = ",") %>%  # Separar por ","
  select(word, sentiment)  # Manter apenas palavras e sentimentos

# 🔄 Converter sentimentos para categórico (positivo, negativo, neutro)
oplexicon$sentiment <- ifelse(oplexicon$sentiment == "-1", "Negativa",
                              ifelse(oplexicon$sentiment == "1", "Positivo", "Neutro"))

# 📢 Exibir as primeiras linhas para ver se o processamento deu certo
head(oplexicon)


# ========================== #
# 📋 PRÉ-PROCESSAMENTO DO TEXTO
# ========================== #

clean_text <- function(text_column) {
  text <- tolower(text_column)  # Converter para minúsculas
  text <- removePunctuation(text)  # Remover pontuação
  text <- removeNumbers(text)  # Remover números
  text <- removeWords(text, stopwords("portuguese"))  # Remover stopwords
  text <- stripWhitespace(text)  # Remover espaços extras
  return(text)
}

# Aplicar a função na variável `resultados`
df$resultados <- sapply(df$resultados, clean_text)

# ========================== #
# ANÁLISE DE SENTIMENTO EM PORTUGUÊS
# ========================== #

# ========================== #
# 📊 CLASSIFICAÇÃO DOS SENTIMENTOS NOS TEXTOS
# ========================== #

# 📌 Tokenizar palavras nos textos
df_tokens <- df %>%
  unnest_tokens(word, resultados)  # Separa os textos em palavras

# 📌 Juntar com o léxico de sentimentos para identificar cada palavra
df_sentimentos <- df_tokens %>%
  inner_join(oplexicon, by = "word") %>%
  count(sentiment) %>%
  rename(Sentimento = sentiment, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1))

# 📌 Criar rótulo no formato "XX% (N)" com os valores reais
df_sentimentos <- df_sentimentos %>%
  mutate(Label = paste0(Porcentagem, "% (", Quantidade, ")"))

# ========================== #
# 📊 VISUALIZAÇÃO DOS SENTIMENTOS
# ========================== #

p <- ggplot(df_sentimentos, aes(x = reorder(Sentimento, -Quantidade), y = Porcentagem, fill = Sentimento)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Label), vjust = 1.5, size = 4, color = "black") +  # 🔹 Rótulo dentro das barras em branco
  scale_fill_manual(values = c("Negativa" = "red", "Positivo" = "green", "Neutro" = "gray")) +  # 🔹 Cores corretas
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +  # 🔹 Eixo Y fixo de 0 a 100
  labs(title = "Análise de Sentimentos nos Resultados",
       x = "Sentimento",
       y = "Porcentagem") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")  # 🔹 Remove legenda pois a cor já indica

p
# Salvar o gráfico em `.jpeg`
ggsave("Substantivo/analise_sentimentos.jpeg", plot = p, width = 8, height = 5, dpi = 300)

#  Exibir os resultados no console
print("Distribuição de Sentimentos nos Resultados")
print(df_sentimentos)



## Latent Dirichlet Allocation (LDA) ##

# Identificação de Temas Principais (LDA)

# ========================== #
#  LIMPEZA E PRÉ-PROCESSAMENTO DOS TEXTOS
# ========================== #

# Carregar bibliotecas necessárias
library(tm)
library(SnowballC)  # Para stemização (reduzir palavras para a raiz)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)
library(forcats)
library(textstem)

# Criar um corpus de textos da variável `resultados`
corpus <- Corpus(VectorSource(df$resultados))

# Lista de palavras irrelevantes (além das stopwords padrão)
stopwords_extras <- c("ser", "estar", "ter", "fazer", "haver", "poder", "dever", "maior",
                      "ir", "vir", "dar", "ficar", "passar", "dizer", "ver", "outro",
                      "assim", "então", "ainda", "também", "cada", "muito", "pouco",
                      "sobre", "entre", "desde", "após", "contra", "perante", "através")

# Aplicar limpeza de texto
corpus <- tm_map(corpus, content_transformer(tolower))  # Converter para minúsculas
corpus <- tm_map(corpus, removePunctuation)  # Remover pontuação
corpus <- tm_map(corpus, removeNumbers)  # Remover números
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))  # Remover stopwords padrão
corpus <- tm_map(corpus, removeWords, stopwords_extras)  # Remover palavras irrelevantes extras
corpus <- tm_map(corpus, stripWhitespace)  # Remover espaços extras
#corpus <- tm_map(corpus, content_transformer(stemDocument))  # Aplicar stemização para normalizar palavras no singular
corpus <- tm_map(corpus, content_transformer(lemmatize_strings))


# Criar uma matriz termo-documento
tdm <- DocumentTermMatrix(corpus)
tdm <- removeSparseTerms(tdm, 0.98)  # Remover termos raros

# ========================== #
#  APLICAR O ALGORITMO LDA

# Definir número de tópicos
num_topics <- 5

# Criar modelo LDA
lda_model <- LDA(tdm, k = num_topics, control = list(seed = 1234))

# Obter palavras-chave para cada tópico
topicos <- tidy(lda_model, matrix = "beta")

# Selecionar as 10 palavras mais representativas por tópico
top_terms <- topicos %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Nomear os tópicos baseados nas palavras mais relevantes
topic_labels <- c(
  "Gestão de Riscos e Desastres",  # Tópico 1
  "Impactos Socioambientais",  # Tópico 2
  "Políticas Públicas e Governança",  # Tópico 3
  "Modelos de Resiliência",  # Tópico 4
  "Resposta e Recuperação Pós-Desastre"  # Tópico 5
)

# Substituir os números dos tópicos pelos nomes textuais
top_terms$topic <- factor(top_terms$topic, 
                          levels = 1:num_topics, 
                          labels = topic_labels)

# 📢 Exibir os temas principais no console
print("Principais Palavras-Chave por Tópico")
print(top_terms)

# ========================== #
#  GRÁFICO DE DISTRIBUIÇÃO DOS TEMAS (COM NOMES)
# ========================== #

# 📊 Criar gráfico ajustado para exibição correta das palavras-chave por tópico
p <- ggplot(top_terms, aes(x = reorder_within(term, beta, topic), y = beta, fill = as.factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = round(beta, 4)), vjust = 0.4, hjust = 0.9, size = 4, color = "black") +  # 🔍 Ajuste da posição dos textos das barras
  facet_wrap(~ topic, scales = "free") +  # Exibe os nomes dos tópicos
  scale_x_reordered() +
  labs(title = "Top 10 Palavras-Chave por Tópico",
       x = "Palavras-Chave",
       y = "Importância no Tópico (Beta)") +  
  coord_flip() +
  theme_minimal(base_size = 12) +  # 🔹 Define tamanho base para 12
  theme(
    strip.text = element_text(size = 14, face = "bold"),  # 🔹 Tamanho dos rótulos dos tópicos aumentado
    axis.text.x = element_text(size = 14),  # 🔹 Tamanho dos números do eixo X
    axis.text.y = element_text(size = 14, face = "bold", hjust = 0.1),  # 🔹 AFASTA os labels do eixo Y
    axis.title.x = element_text(size = 14, face = "bold"),  
    axis.title.y = element_text(size = 14, face = "bold"),  
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # 🔹 Título centralizado e maior
  )

# 📌 Exibir o gráfico
print(p)

#  Salvar o gráfico em `.jpeg`
ggsave("Substantivo/topic_modeling.jpeg", plot = p, width = 20, height = 12, dpi = 300)




# ========================== #
#     Rede de Coocorrência 
# 
# ========================== #


# PRÉ-PROCESSAMENTO DO TEXTO
# Criar função para limpar o texto
clean_text <- function(text_column) {
  text <- tolower(text_column)  # Converter para minúsculas
  text <- removePunctuation(text)  # Remover pontuação
  text <- removeNumbers(text)  # Remover números
  text <- removeWords(text, stopwords("portuguese"))  # Remover stopwords em português
  text <- stripWhitespace(text)  # Remover espaços extras
  return(text)
}

# Aplicar a função na variável `resultados`
df$resultados <- sapply(df$resultados, clean_text)

# ========================== #
# CRIAR PARES DE PALAVRAS (BIGRAMAS)


# 🔄 Separar os textos em pares de palavras (bigramas)
bigramas <- df %>%
  unnest_tokens(bigram, resultados, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("palavra1", "palavra2"), sep = " ") %>%
  filter(!is.na(palavra1) & !is.na(palavra2))  # Remover valores NA

# 🔄 Remover preposições, artigos e verbos comuns manualmente
palavras_irrelevantes <- c("de", "estudo", "apesar","recomenda", "da", "do", "para", "com", "por", "sobre", "entre", "a", "o", "e", "as", "os", 
                           "que", "ainda","é", "um", "uma", "seu", "sua", "ser", "ter", "está", "foi", "vai", "pode", "não")

bigramas <- bigramas %>%
  filter(!palavra1 %in% palavras_irrelevantes & !palavra2 %in% palavras_irrelevantes)

# Contar as coocorrências dos bigramas
bigram_freq <- bigramas %>%
  count(palavra1, palavra2, sort = TRUE) %>%
  filter(n > 3)  # 🔍 Filtrar apenas bigramas que aparecem pelo menos 5 vezes para eliminar ruído

# ========================== #
# CRIAR A REDE DE PALAVRAS (COM FILTRO)


# Criar um grafo a partir dos bigramas
grafo <- graph_from_data_frame(bigram_freq)

# ========================== #
#  VISUALIZAR A REDE DE PALAVRAS 


p <- ggraph(grafo, layout = "fr") +  # Layout de força (fruchterman-reingold)
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, color = "gray70") +  # Linhas de conexão mais suaves
  geom_node_point(size = 5.5, color = "darkblue") +  # 🔍 Aumentar nós mais importantes
  geom_node_text(aes(label = name), repel = TRUE, size = 5, color = "black") +  # 🔍 Evitar sobreposição de palavras
  labs(title = "Rede de Coocorrência de Palavras nos Resultados") +
  theme_void()

p

#  Salvar o gráfico em `.jpeg`
ggsave("Substantivo/rede_coocorrencia_filtrada.jpeg", plot = p, width = 10, height = 6, dpi = 300)



## Classificação dos Resultados dos Textos de acordo com o tipo e modelo
## De gestão de desastre


# ========================== #
#  PRÉ-PROCESSAMENTO DOS TEXTOS

#  Criar função para limpar os textos
clean_text <- function(text_column) {
  text <- tolower(text_column)  # Converter para minúsculas
  text <- removePunctuation(text)  # Remover pontuação
  text <- removeNumbers(text)  # Remover números
  text <- removeWords(text, stopwords("portuguese"))  # Remover stopwords
  text <- stripWhitespace(text)  # Remover espaços extras
  return(text)
}

# Aplicar a função na variável `resultados`
df$resultados_limpos <- sapply(df$resultados, clean_text)

# ========================== #
#  PALAVRAS-CHAVE PARA CLASSIFICAÇÃO
# ========================== #

#  Criar dicionário de palavras-chave para cada categoria

# Etapas do Desastre
etapas_keywords <- list(
  "Pré-aguda" = c("mitigação", "prevenção", "planejamento", "redução", "monitoramento"),
  "Crise-aguda" = c("emergência", "resgate", "desastre", "evacuação", "urgência"),
  "Pós-crise" = c("reconstrução", "recuperação", "adaptação", "resiliência"),
  "Outro" = c("educação", "tecnologia", "inovação", "participação")
)

# 🔄 Criar uma função para contar palavras por categoria e evitar valores vazios
classificar_etapa <- function(texto) {
  contagem <- sapply(etapas_keywords, function(palavras) sum(str_count(texto, palavras)))
  if (all(contagem == 0)) {
    return("Não Classificado")  # Se nenhuma palavra for encontrada, atribuir "Não Classificado"
  } else {
    return(names(which.max(contagem)))  # Retorna a categoria com maior frequência de palavras
  }
}

# 📊 Aplicar a função para classificar os textos na etapa do desastre
library(stringr)
df$etapa_classificada <- vapply(df$resultados_limpos, classificar_etapa, FUN.VALUE = character(1))  # 🔍 vapply() para garantir saída de texto

# ========================== #
# 🏢 Modelos de Gestão do Desastre
# ========================== #

modelos_keywords <- list(
  "Modelo Cíclico" = c("prevenção", "preparação", "resposta", "recuperação"),
  "Modelo de Emergência" = c("urgência", "emergência", "socorro", "ações imediatas"),
  "Governança Multissetorial" = c("governança", "parceria", "setor público", "participação"),
  "Modelo Prospectivo/Corretivo" = c("planejamento", "adaptação", "mitigação", "correção")
)

# 🔄 Criar função para classificar o modelo de gestão
classificar_modelo <- function(texto) {
  contagem <- sapply(modelos_keywords, function(palavras) sum(str_count(texto, palavras)))
  if (all(contagem == 0)) {
    return("Não Classificado")  # Se nenhuma palavra for encontrada, atribuir "Não Classificado"
  } else {
    return(names(which.max(contagem)))  # Retorna a categoria com maior frequência de palavras
  }
}

# 📊 Aplicar a função para classificar os textos no modelo de gestão
df$modelo_classificado <- vapply(df$resultados_limpos, classificar_modelo, FUN.VALUE = character(1))  # 🔍 vapply() para garantir saída de texto

# ========================== #
# 📋 TABELA FINAL CLASSIFICADA
# ========================== #

df_classificado <- df %>%
  select(resultados, etapa_classificada, modelo_classificado)

#  Salvar a tabela classificada como `.xlsx`
export(df_classificado, "Classificacao/textos_classificados.xlsx")

# 🔹 Exibir uma amostra da tabela classificada
print(head(df_classificado, 10))

# ========================== #
# CÁLCULO DE FREQUÊNCIA EM PORCENTAGEM
# ========================== #


df <- df %>%
  mutate(
    etapa_classificada = recode(etapa_classificada, 
                                "Outro" = "Mais de 1", 
                                "Não Classificado" = "Não se Aplica"),
    modelo_classificado = recode(modelo_classificado, 
                                 "Não Classificado" = "Não se Aplica")
  )

# 🔹 Frequência de `etapa_classificada`
freq_etapa <- df %>%
  count(etapa_classificada) %>%
  mutate(Porcentagem = round((n / sum(n)) * 100, 1))

# 🔹 Frequência de `modelo_classificado`
freq_modelo <- df %>%
  count(modelo_classificado) %>%
  mutate(Porcentagem = round((n / sum(n)) * 100, 1))

# 🔹 Exibir as frequências
print("📊 Frequência - Etapa do Desastre")
print(freq_etapa)

print("📊 Frequência - Modelo de Gestão")
print(freq_modelo)


# ========================== #
# 📋 AGRUPAR TEXTOS POR CATEGORIA
# ========================== #

df_classificado <- df %>%
  select(autor, Ano_publicacao, resultados, etapa_classificada, modelo_classificado) %>%
  arrange(etapa_classificada, modelo_classificado)

# ========================== #
#  RECODIFICAÇÃO DOS AUTORES


#  Função para ajustar os nomes dos autores corretamente
formatar_autores <- function(autor, ano) {
  if (is.na(autor) | autor == "") {
    return("Autor Desconhecido")
  }
  
  # Substituir " e " por "," para tratar os casos onde "e" é usado como separador
  autor_tratado <- gsub(" e ", ",", autor)
  
  # Separar os autores em uma lista
  autores <- unlist(strsplit(autor_tratado, ","))
  
  # Remover espaços extras
  autores <- trimws(autores)
  
  # 📌 Se há apenas um nome, pegar o último sobrenome apenas
  if (length(autores) == 1) {
    palavras <- unlist(strsplit(autores[1], " "))  # Separar em palavras
    ultimo_sobrenome <- tail(palavras, 1)  # Última palavra do nome (sobrenome)
    return(paste(ultimo_sobrenome, "(", ano, ")"))
  }
  
  # 📌 Se há dois autores, pegar o último sobrenome de cada um
  sobrenomes <- sapply(autores, function(a) {
    palavras <- unlist(strsplit(a, " "))  # Separar em palavras
    return(tail(palavras, 1))  # Pegar a última palavra (sobrenome)
  })
  
  if (length(sobrenomes) == 2) {
    return(paste(sobrenomes[1], "e", sobrenomes[2], "(", ano, ")"))
  }
  
  # 📌 Se há mais de dois autores, usar "et al."
  return(paste(sobrenomes[1], "et al.", "(", ano, ")"))
}

# 📊 Aplicar a função na variável `autor` e criar uma nova coluna `autor_formatado`
df <- df %>%
  mutate(autor_formatado = mapply(formatar_autores, autor, Ano_publicacao))

# ========================== #
# 📋 AGRUPAMENTO DOS TEXTOS POR CLASSIFICAÇÃO

df_classificado <- df %>%
  select(autor, autor_formatado, Ano_publicacao, resultados, etapa_classificada, modelo_classificado) %>%
  arrange(etapa_classificada, modelo_classificado)

# 📂 Salvar a tabela classificada como `.xlsx`
export(df_classificado, "Classificacao/textos_classificados_ajustado.xlsx")

# 🔹 Exibir uma amostra da tabela classificada
print(head(df_classificado, 10))


# ========================== #
# 📋 TABELA AGRUPADA - ETAPA DO DESASTRE x AUTORES

library(dplyr)
library(gt)

tabela_etapa_autores <- df_classificado %>%
  group_by(etapa_classificada) %>%
  summarise(Autores = paste(unique(autor_formatado), collapse = ", ")) %>%
  gt() %>%
  tab_header(title = "Autores por Etapa do Desastre") %>%
  cols_label(etapa_classificada = "Etapa do Desastre", Autores = "Lista de Autores") %>%
  cols_align(align = "center", columns = c(etapa_classificada, Autores)) %>%  # 🔍 Centralizar títulos das colunas
  tab_options(
    table.font.names = "Times New Roman",  # 🔍 Aplicar fonte Times New Roman
    heading.align = "center",  # 🔍 Centralizar título da tabela
    column_labels.font.weight = "bold",  # 🔍 Negrito nos rótulos das colunas
    table.font.size = px(12)  # 🔍 Ajustar tamanho da fonte
  )

# 📢 Exibir a tabela
print(tabela_etapa_autores)

# ========================== #
# TABELA AGRUPADA - MODELO DE GESTÃO x AUTORES


# Criar tabela agrupando autores por Modelo de Gestão
tabela_modelo_autores <- df_classificado %>%
  group_by(modelo_classificado) %>%
  summarise(Autores = paste(unique(autor_formatado), collapse = ", ")) %>%
  gt() %>%
  tab_header(title = "Autores por Modelo de Gestão") %>%
  cols_label(modelo_classificado = "Modelo de Gestão", Autores = "Lista de Autores") %>%
  cols_align(align = "center", columns = c(modelo_classificado, Autores)) %>%  # 🔍 Centralizar títulos das colunas
  tab_options(
    table.font.names = "Times New Roman",  # 🔍 Aplicar fonte Times New Roman
    heading.align = "center",  # 🔍 Centralizar título da tabela
    column_labels.font.weight = "bold",  # 🔍 Negrito nos rótulos das colunas
    table.font.size = px(12)  # 🔍 Ajustar tamanho da fonte
  )

print(tabela_modelo_autores)
