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
  library(stringr)
  library(ggplot2)
  library(readxl)
})

df <- read_excel(data_path)
names(df) <- make.names(names(df), unique = TRUE)

split_and_clean <- function(column) {
  df_long <- df %>%
    select(all_of(column)) %>%
    filter(!is.na(!!sym(column))) %>%
    mutate(!!sym(column) := tolower(!!sym(column))) %>%
    separate_rows(!!sym(column), sep = ",") %>%
    mutate(!!sym(column) := trimws(!!sym(column))) %>%
    filter(!!sym(column) != "" & !!sym(column) != "na") %>%
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
      # CORRECAO (Avaliador B): categoria renomeada de "desastres naturais" para "desastres",
      # para nao reforcar a nocao acritica de desastre como fenomeno puramente natural.
      "impactos ambientais" = "desastres",
      "gestão de riscos ambientais" = "desastres",
      "gestão integral de riscos de desastres naturais" = "desastres",
      "gestão de desastres" = "desastres",
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
      "derramamento de petróleo" = "derramamento de produtos químico",
      "riscos derivados do uso da ciência e tecnologia" = "desastres tecnológicos",
      "ocupação de áreas suscetíveis a deslizamento" = "deslizamento"
    ))) %>%
    # Segunda passagem deliberada: corrige resíduos que o str_replace_all()
    # deixa quando uma substituição cria texto que também precisaria ser mapeado.
    mutate(!!sym(column) := recode(!!sym(column),
      "desastres naturais" = "desastres",
      "deslizamentos" = "deslizamento",
      "deslizamento de terra" = "deslizamento",
      "rompimento de barragem de mineração" = "rompimento de barragem",
      "incêndios florestais florestais" = "incêndios florestais",
      .default = !!sym(column)
    ))
  return(df_long)
}

desastre_df <- split_and_clean("tipo_desastre")

desastre_freq_completa <- desastre_df %>%
  count(tipo_desastre, sort = TRUE) %>%
  rename(Tipo_Desastre = tipo_desastre, Quantidade = n) %>%
  mutate(Porcentagem = round((Quantidade / sum(Quantidade)) * 100, 1))

stopifnot(
  sum(desastre_freq_completa$Quantidade) == 98,
  nrow(desastre_freq_completa) == 23,
  desastre_freq_completa$Quantidade[desastre_freq_completa$Tipo_Desastre == "desastres"] == 5
)

desastre_freq <- desastre_freq_completa %>% head(10)

desastre_freq$Tipo_Desastre <- str_to_title(desastre_freq$Tipo_Desastre)

cat("=== Top 10 Tipos de Desastre (categoria renomeada) ===\n")
print(desastre_freq)
cat("Total auditado: 98 ocorrências em 23 categorias; Desastres = 5 (5,1%).\n")

p <- ggplot(desastre_freq, aes(x = reorder(Tipo_Desastre, -Porcentagem), y = Porcentagem)) +
  geom_bar(stat = "identity", fill = "gray70", color = "black") +
  geom_text(aes(label = paste0(Porcentagem, "% (", Quantidade, ")")), vjust = -0.5, size = 4) +
  labs(title = "Tipos de Desastre mais Frequentes na Amostra (Top 10)",
       subtitle = "Categoria \"Desastres Naturais\" renomeada para \"Desastres\"",
       x = NULL, y = "Porcentagem") +
  ylim(0, max(desastre_freq$Porcentagem) * 1.2) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        plot.background = element_rect(fill = "white", color = NA))

ggsave(file.path(output_dir, "grafico_tipo_desastre_renomeado.png"), p, width = 10, height = 7, dpi = 300)
cat("\nOK - grafico salvo\n")
