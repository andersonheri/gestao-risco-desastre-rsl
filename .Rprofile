local_library <- file.path(getwd(), ".r-library")
if (dir.exists(local_library)) {
  .libPaths(c(local_library, .libPaths()))
}

if (.Platform$OS.type == "windows") {
  invisible(suppressWarnings(Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.utf8")))
}
