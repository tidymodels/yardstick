get_col <- function(x, val) {
  call <- match.call()
  if(!any(colnames(x) == val))
    stop("Column `", call[["val"]], "`` is not in `",
         call[["x"]], "`", call. = FALSE)
  getElement(x, val)
}


pos_val <- function(xtab, check = TRUE) {
  if (check)
    if (!all(dim(xtab) == 2))
      stop("Only relavant for 2x2 tables", call. = TRUE)
  
  if (getOption("yardstick.event_first"))
    colnames(xtab)[1]
  else
    colnames(xtab)[2]
}

neg_val <- function(xtab, check = TRUE) {
  if (check)
    if (!all(dim(xtab) == 2))
      stop("Only relavant for 2x2 tables", call. = TRUE)
  
  if (getOption("yardstick.event_first"))
    colnames(xtab)[2]
  else
    colnames(xtab)[1]
}


check_table <- function(x) {
  if (!all.equal(nrow(x), ncol(x)))
    stop("the table must have nrow = ncol", call. = FALSE)
  if (!isTRUE(all.equal(rownames(x), colnames(x))))
    stop("the table must the same groups in the same order", call. = FALSE)
  invisible(NULL)
}


# tmp <- as.table(matrix(c(11, 12, 13, 21, 22, 23, 31, 32, 33), byrow = FALSE, ncol = 3))
# use in test
flatten <- function(xtab) {
  p <- ncol(xtab)
  if(nrow(xtab) != p)
    stop("table must have equal dimensions")
  flat <- as.vector(xtab)
  names(flat) <- paste("cell", rep(1:p, p), rep(1:p, each = p), sep = "_")
  flat
}

match_levels_to_cols <- function(nms, lvl) {
  if(length(nms) == 1)
    return(nms)
  if(length(nms) > 2)
    stop("These functions are for two-class problems and there ",
         "were ", length(nms), " columns given in `...`",
         call. = FALSE)
  
  common <- lvl[lvl %in% nms]
  if(length(common) == 0)
    stop("Multiple columns were specified for class probabilities ",
         "but none match the levels of `truth`. Does not compute. ",
         "Does not compuuuuu", call. = FALSE)
  warning("Multiple columns were specified for class probabilities; ",
          common[1], " will be used.", call. = FALSE)
  common[1]
}
