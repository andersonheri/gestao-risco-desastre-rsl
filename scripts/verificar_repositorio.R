args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
if (length(script_arg) != 1) stop("Execute este arquivo com Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_library <- file.path(project_root, ".r-library")
if (dir.exists(local_library)) .libPaths(c(local_library, .libPaths()))

required_files <- c(
  "README.md",
  "dados/banco_final.xlsx",
  "scripts/analise_completa.R",
  "scripts/reproduzir_figuras_revisadas.R",
  "scripts/grafico_tipos_desastre.R",
  "scripts/grafico_clusters_coautoria.R",
  "scripts/diagnostico_clusters.R"
)
missing_files <- required_files[!file.exists(file.path(project_root, required_files))]
if (length(missing_files)) stop("Arquivos ausentes: ", paste(missing_files, collapse = ", "))

r_scripts <- list.files(file.path(project_root, "scripts"), pattern = "[.]R$", full.names = TRUE)
parse_errors <- character()
for (script in r_scripts) {
  tryCatch(parse(script, encoding = "UTF-8"), error = function(e) {
    parse_errors <<- c(parse_errors, paste(basename(script), conditionMessage(e)))
  })
}
if (length(parse_errors)) stop("Erros de sintaxe: ", paste(parse_errors, collapse = "; "))

packages <- c(
  "dplyr", "forcats", "ggraph", "ggplot2", "gt", "igraph", "readxl",
  "reshape2", "rio", "RColorBrewer", "skimr", "SnowballC", "stringr", "textstem",
  "tidyr", "tidytext", "tm", "topicmodels", "wordcloud"
)
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Dependências ausentes: ", paste(missing_packages, collapse = ", "))

data <- readxl::read_excel(file.path(project_root, "dados", "banco_final.xlsx"))
if (nrow(data) != 50) stop("O banco deveria ter 50 estudos; foram encontrados ", nrow(data), ".")
required_columns <- c("titulo", "autor", "Ano_publicacao", "tipo_desastre", "resultados")
if (!all(required_columns %in% names(data))) {
  stop("Colunas essenciais ausentes: ", paste(setdiff(required_columns, names(data)), collapse = ", "))
}

cat("OK - estrutura, banco, sintaxe e dependências verificados.\n")
