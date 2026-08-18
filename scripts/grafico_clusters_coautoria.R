args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
if (length(script_arg) != 1) stop("Execute este arquivo com Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
data_path <- file.path(project_root, "dados", "banco_final.xlsx")
output_dir <- file.path(project_root, "resultados", "figuras")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(igraph)
  library(ggraph)
  library(ggplot2)
  library(readxl)
})

df <- read_excel(data_path)
names(df) <- make.names(names(df), unique = TRUE)

df <- df %>% mutate(ID_Publicacao = row_number())
df <- df %>% mutate(autor = gsub(" e ", ",", autor))

df_autores <- df %>%
  select(ID_Publicacao, autor) %>%
  separate(autor, into = paste0("autor", 1:7), sep = ",", fill = "right", extra = "drop")
df_autores <- left_join(df, df_autores, by = "ID_Publicacao")

df_long <- df_autores %>%
  pivot_longer(cols = starts_with("autor"), names_to = "autor_num", values_to = "autor") %>%
  filter(!is.na(autor) & autor != "")

coautor_edges <- df_long %>%
  group_by(ID_Publicacao) %>%
  summarise(
    pares = list(if (n() > 1) combn(autor, 2, FUN = function(x) paste(x, collapse = " - ")) else NA_character_),
    .groups = "drop"
  ) %>%
  unnest(pares) %>%
  filter(!is.na(pares)) %>%
  separate(pares, into = c("autor1", "autor2"), sep = " - ")

grafo <- graph_from_data_frame(coautor_edges, directed = FALSE)

# CORRECAO: usar os clusters REAIS do Louvain, sem forcar em 5 grupos via cut()
set.seed(42)
clusters <- cluster_louvain(grafo, resolution = 0.4)
df_clusters <- data.frame(
  autor = V(grafo)$name,
  cluster_real = as.integer(membership(clusters))
)

tamanhos <- df_clusters %>% count(cluster_real, name = "n_autores") %>% arrange(desc(n_autores))

# Definir os N_DESTACADOS maiores clusters reais para exibir individualmente;
# o restante (grupos pequenos, em geral equipe de autoria de um unico artigo)
# e agregado em "Demais Grupos".
N_DESTACADOS <- 7
top_clusters <- tamanhos$cluster_real[1:N_DESTACADOS]

df_clusters <- df_clusters %>%
  mutate(grupo_exibicao = ifelse(cluster_real %in% top_clusters,
                                  paste0("Grupo ", match(cluster_real, top_clusters)),
                                  "Demais Grupos"))

V(grafo)$grupo_exibicao <- df_clusters$grupo_exibicao[match(V(grafo)$name, df_clusters$autor)]

autores_principais <- unique(df_autores$autor1)

cores <- c(setNames(RColorBrewer::brewer.pal(N_DESTACADOS, "Set1"), paste0("Grupo ", 1:N_DESTACADOS)),
           "Demais Grupos" = "gray80")

p <- ggraph(grafo, layout = "fr") +
  geom_edge_link(alpha = 0.3, color = "gray70") +
  geom_node_point(aes(color = grupo_exibicao), size = 4.5) +
  geom_node_text(aes(label = ifelse(name %in% autores_principais & grupo_exibicao != "Demais Grupos", name, "")),
                 repel = TRUE, size = 3.6, color = "black", max.overlaps = 30) +
  scale_color_manual(values = cores) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Rede de Coautoria — Clusters Reais (Louvain, sem corte forçado)",
       subtitle = paste0(N_DESTACADOS, " maiores grupos reais destacados; demais ", 
                          nrow(tamanhos) - N_DESTACADOS, " grupos (2-6 autores cada) agregados em cinza"),
       color = "Grupo")

ggsave(file.path(output_dir, "grafico6_clusters_reais.png"), p, width = 12, height = 8, dpi = 300)

# Tabela-resumo para o texto/Quadro
resumo <- df_clusters %>%
  group_by(grupo_exibicao) %>%
  summarise(n_autores = n(), .groups = "drop")

dens_por_grupo <- sapply(top_clusters, function(cl) {
  membros <- df_clusters$autor[df_clusters$cluster_real == cl]
  sub <- induced_subgraph(grafo, vids = which(V(grafo)$name %in% membros))
  round(edge_density(sub), 2)
})
resumo_top <- data.frame(
  grupo = paste0("Grupo ", 1:N_DESTACADOS),
  cluster_louvain_original = top_clusters,
  n_autores = tamanhos$n_autores[1:N_DESTACADOS],
  densidade_interna = dens_por_grupo
)

cat("=== Resumo dos", N_DESTACADOS, "maiores grupos reais ===\n")
print(resumo_top)
cat("\n=== Demais grupos (agregados) ===\n")
cat("Numero de grupos:", nrow(tamanhos) - N_DESTACADOS, "\n")
cat("Total de autores nesses grupos:", sum(tamanhos$n_autores[-(1:N_DESTACADOS)]), "\n")
cat("Tamanho desses grupos: min =", min(tamanhos$n_autores[-(1:N_DESTACADOS)]),
    " max =", max(tamanhos$n_autores[-(1:N_DESTACADOS)]),
    " mediana =", median(tamanhos$n_autores[-(1:N_DESTACADOS)]), "\n")
cat("\nTotal de clusters reais encontrados pelo Louvain:", nrow(tamanhos), "\n")
cat("Total de autores na rede:", vcount(grafo), "\n")
cat("Modularidade:", round(modularity(clusters), 3), "\n")
