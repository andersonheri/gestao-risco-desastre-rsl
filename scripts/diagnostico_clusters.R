args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
if (length(script_arg) != 1) stop("Execute este arquivo com Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
data_path <- file.path(project_root, "dados", "banco_final.xlsx")
output_dir <- file.path(project_root, "resultados", "auditoria")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(igraph)
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

grafo <- graph_from_data_frame(coautor_edges, directed = FALSE)
cat("Numero de nos (autores):", vcount(grafo), "\n")
cat("Numero de arestas:", ecount(grafo), "\n")

clusters <- cluster_louvain(grafo, resolution = 0.4)
df_clusters <- data.frame(
  autor = V(grafo)$name,
  cluster_real = as.integer(membership(clusters))
)

num_clusters_real <- length(unique(df_clusters$cluster_real))
cat("\n=== CLUSTERS REAIS (Louvain, sem corte forcado) ===\n")
cat("Numero de clusters encontrados pelo Louvain:", num_clusters_real, "\n")
cat("Modularidade:", modularity(clusters), "\n\n")

tab_real <- df_clusters %>% count(cluster_real, name = "n_autores") %>% arrange(desc(n_autores))
print(tab_real)

# Densidade interna (numero de arestas internas / possiveis) por cluster real
dens_real <- sapply(sort(unique(df_clusters$cluster_real)), function(cl) {
  membros <- df_clusters$autor[df_clusters$cluster_real == cl]
  sub <- induced_subgraph(grafo, vids = which(V(grafo)$name %in% membros))
  n <- vcount(sub); e <- ecount(sub)
  dens <- if (n > 1) edge_density(sub) else NA
  c(cluster = cl, n_autores = n, arestas_internas = e, densidade = round(dens, 4))
})
cat("\n=== Densidade interna por cluster REAL ===\n")
print(t(dens_real))

# Agora replicando o CUT forcado em 5 grupos, como no script original
df_clusters_cut <- df_clusters
if (num_clusters_real > 5) {
  df_clusters_cut$cluster_cut <- as.integer(as.character(cut(df_clusters_cut$cluster_real, breaks = 5, labels = 1:5)))
} else {
  df_clusters_cut$cluster_cut <- df_clusters_cut$cluster_real
}

cat("\n=== Como o CUT() forcado em 5 misturou os clusters reais ===\n")
tab_cross <- table(cluster_real = df_clusters_cut$cluster_real, cluster_apos_cut = df_clusters_cut$cluster_cut)
print(tab_cross)

dens_cut <- sapply(sort(unique(df_clusters_cut$cluster_cut)), function(cl) {
  membros <- df_clusters_cut$autor[df_clusters_cut$cluster_cut == cl]
  sub <- induced_subgraph(grafo, vids = which(V(grafo)$name %in% membros))
  n <- vcount(sub); e <- ecount(sub)
  dens <- if (n > 1) edge_density(sub) else NA
  c(cluster_pos_cut = cl, n_autores = n, arestas_internas = e, densidade = round(dens, 4))
})
cat("\n=== Densidade interna por cluster APOS o cut() forcado (o que o texto atual descreve) ===\n")
print(t(dens_cut))

saveRDS(list(grafo=grafo, df_clusters=df_clusters, df_clusters_cut=df_clusters_cut),
        file.path(output_dir, "cluster_diag.rds"))
