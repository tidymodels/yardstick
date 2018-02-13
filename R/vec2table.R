# converts two vectors to a common table format and checks the
# class levels in various ways

vec2table <-
  function(truth,
           estimate,
           na.rm = TRUE,
           dnn = c("Prediction", "Truth"),
           two_class  = FALSE,
           ...) {
    if (!is.factor(truth)) {
      truth <- factor(truth)
      warning("`truth` was converted to a factor", call. = TRUE)
    }
    if (!is.factor(estimate)) {
      if (!isTRUE(all.equal(sort(unique(estimate)), sort(levels(truth)))))
        stop("There are different possible values in `truth` and `estimate`")
      estimate <- factor(estimate, levels = levels(truth))
      warning("`estimate` was converted to a factor", call. = TRUE)
    }

    if (na.rm) {
      cc <- complete.cases(estimate) & complete.cases(truth)
      if (any(!cc)) {
        estimate <- estimate[cc]
        truth <- truth[cc]
        if(all(!cc))
          stop("All rows have at least one missing value",
               call. = FALSE)
      }
    }
    est_lev <- levels(estimate)
    tru_lev <- levels(truth)

    if (length(est_lev) != length(tru_lev))
      stop("The columns containing the predicted and true ",
           "classes must have the same levels.", call. = FALSE)

    if (!any(est_lev %in% tru_lev)) {
      stop("`estimate` must contain levels that overlap `truth`.",
           call. = FALSE)
    }

    if (any(tru_lev != est_lev)) {
      stop(
        "Levels are not in the same (or in the same order) for ",
        "true and predicted classes. ",
        call. = FALSE
      )
    }

    if (length(est_lev) < 2)
      stop("There must be at least 2 factors levels in the ",
           "column containing the predicted class",
           call. = FALSE)

    xtab <- table(estimate, truth, dnn = dnn, ...)

    xtab
  }
