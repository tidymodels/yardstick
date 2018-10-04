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
      stop("Only relevant for 2x2 tables", call. = TRUE)

  if (getOption("yardstick.event_first"))
    colnames(xtab)[1]
  else
    colnames(xtab)[2]
}

neg_val <- function(xtab, check = TRUE) {
  if (check)
    if (!all(dim(xtab) == 2))
      stop("Only relevant for 2x2 tables", call. = TRUE)

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


# dplyr:::tbl_at_vars
tbl_at_vars <- function (tbl, vars, .include_group_vars = FALSE) {

  if (.include_group_vars) {
    tibble_vars <- dplyr::tbl_vars(tbl)
  }
  else {
    tibble_vars <- dplyr::tbl_nongroup_vars(tbl)
  }

  if (rlang::is_null(vars)) {
    character()
  }
  else if (rlang::is_character(vars)) {
    vars
  }
  else if (rlang::is_integerish(vars)) {
    tibble_vars[vars]
  }
  # make tbl_at_vars() more flexible so it allows a single quosure
  else if (rlang::is_quosure(vars)) {
    out <- tidyselect::vars_select(tibble_vars, !!vars)
    if (!any(rlang::have_name(vars))) {
      names(out) <- NULL
    }
    out
  }
  else if (rlang::is_quosures(vars)) {
    out <- tidyselect::vars_select(tibble_vars, !!!vars)
    if (!any(rlang::have_name(vars))) {
      names(out) <- NULL
    }
    out
  }
  else {
    rlang::abort("`.vars` must be a character/numeric vector or a `vars()` object, not XYZthing")
  }
}

# take data and estimate which can be:
# - quosure with a name, - quosure with a call
# if with a call, evaluate it to get
# - character vector
# - numeric vector
# - quosures object
# then get the character column names
# then construct something we can pass to summarise()
vars_prep <- function(data, estimate) {

  # eval if not a single name
  if(rlang::is_call(rlang::get_expr(estimate))) {
    estimate <- rlang::eval_tidy(estimate)
  }

  chr_vars <- tbl_at_vars(data, estimate)
  sym_vars <- rlang::syms(chr_vars)

  # multiple columns get returned as a matrix, single columns are a vector
  if(length(sym_vars) > 1) {
    estimate <- rlang::quo(matrix(c(!!! sym_vars), ncol = !!length(sym_vars)))
  } else {
    estimate <- sym_vars[[1]]
  }

  estimate
}

add_class <- function(data, cls) {
  class(data) <- c(cls, class(data))
  data
}

construct_name <- function(nm, averaging) {
  if(averaging == "binary") {
    nm
  } else {
    paste0(nm, "_", averaging)
  }
}



