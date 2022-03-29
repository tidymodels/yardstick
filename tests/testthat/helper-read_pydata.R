read_pydata <- function(py_path) {
  py_path <- paste0(py_path, ".rds")
  py_path <- test_path("py-data", py_path)
  readRDS(py_path)
}
