#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import vctrs
#' @importFrom dplyr as_tibble
#' @importFrom lifecycle deprecated
#' @useDynLib yardstick, .registration = TRUE
## usethis namespace: end
NULL

# Importing something from utils so we don't get dinged about having an
# Import we don't use. We use `utils::globalVariables()` at a global scope, and
# R CMD check doesn't detect that. Usually shows up as a NOTE on rhub's Linux
# check machines.
#' @importFrom utils globalVariables
NULL
