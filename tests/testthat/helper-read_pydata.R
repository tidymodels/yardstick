read_pydata <- function(py_path) {
  # depending on if testing one at a time or running
  # CTRL+SHIFT+T
  if(interactive()) {
    # interactive + local use. devtools::test() interactively
    if (Sys.getenv("NOT_CRAN") == "true") {
      path <- "../pycompare/"
    } else {
      path <- "tests/pycompare/"
    }
  } else {
    path <- "../pycompare/"
  }
  readRDS(paste0(path, py_path))
}
