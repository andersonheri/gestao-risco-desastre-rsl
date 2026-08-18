args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", args_all, value = TRUE)
if (length(script_arg) != 1) stop("Execute este arquivo com Rscript.")
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
project_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

local_library <- file.path(project_root, ".r-library")
dir.create(local_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(local_library, .libPaths()))

packages <- c(
  "dplyr", "forcats", "ggraph", "ggplot2", "gt", "igraph", "readxl",
  "reshape2", "rio", "RColorBrewer", "skimr", "SnowballC", "stringr", "textstem",
  "tidyr", "tidytext", "tm", "topicmodels", "wordcloud"
)

repository <- "https://cloud.r-project.org"
available <- available.packages(repos = repository)
dependencies <- unique(unlist(tools::package_dependencies(
  packages,
  db = available,
  which = c("Depends", "Imports", "LinkingTo"),
  recursive = TRUE
)))
all_required <- unique(c(packages, dependencies))
all_required <- intersect(all_required, rownames(available))
missing <- all_required[!file.exists(file.path(local_library, all_required, "DESCRIPTION"))]
if (length(missing)) {
  install.packages(missing, repos = repository, lib = local_library, dependencies = FALSE)
}

still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(still_missing)) {
  stop("Não foi possível instalar: ", paste(still_missing, collapse = ", "))
}

writeLines(capture.output(sessionInfo()), file.path(project_root, "sessionInfo.txt"))
cat("OK - dependências disponíveis em", local_library, "\n")
