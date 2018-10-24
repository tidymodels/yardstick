# roc_df -----------------------------------------------------------------------

#' @rdname roc_auc
autoplot.roc_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`

  # Base chart
  roc_chart <- ggplot2::ggplot(data = object)

  # Add in group interactions if required
  if (inherits(object, "grouped_roc_df")) {

    grps <- dplyr::groups(object)

    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")

    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    roc_chart <- roc_chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {

    interact_expr <- list()

  }

  # splice in the group interactions, or do nothing
  aes_spliced <- ggplot2::aes(
    x = 1 - specificity,
    y = sensitivity,
    !!! interact_expr
  )

  # build the graph
  roc_chart <- roc_chart %+%
    ggplot2::geom_path(mapping = aes_spliced) %+%
    ggplot2::geom_abline(lty = 3) %+%
    ggplot2::coord_equal() %+%
    ggplot2::theme_bw()

  # If we have .level, that means this was multiclass
  # and we want to show 1 vs all graphs
  if (".level" %in% colnames(object)) {
    roc_chart <- roc_chart %+%
      ggplot2::facet_wrap(~.level)
  }

  roc_chart
}

# pr_df ------------------------------------------------------------------------

#' @rdname roc_auc
autoplot.pr_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`

  # Base chart
  pr_chart <- ggplot2::ggplot(data = object)

  # Add in group interactions if required
  if (inherits(object, "grouped_pr_df")) {

    grps <- dplyr::groups(object)

    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")

    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    pr_chart <- pr_chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {

    interact_expr <- list()

  }

  # splice in the group interactions, or do nothing
  aes_spliced <- ggplot2::aes(
    x = recall,
    y = precision,
    !!! interact_expr
  )

  # build the graph
  pr_chart <- pr_chart %+%
    ggplot2::geom_path(mapping = aes_spliced) %+%
    ggplot2::coord_equal() %+%
    ggplot2::theme_bw()

  # If we have .level, that means this was multiclass
  # and we want to show 1 vs all graphs
  if (".level" %in% colnames(object)) {
    pr_chart <- pr_chart %+%
      ggplot2::facet_wrap(~.level)
  }

  pr_chart
}

# gain_df ----------------------------------------------------------------------

# dynamically exported in .onLoad()

#' @rdname gain_curve
autoplot.gain_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`
  `%>%` <- dplyr::`%>%`

  # Base chart
  chart <- ggplot2::ggplot(data = object)

  # Grouped specific chart features
  if (dplyr::is_grouped_df(object)) {

    # Construct the color interaction group
    grps <- dplyr::groups(object)
    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    # Add group legend label
    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")
    chart <- chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {
    interact_expr <- list()
  }

  # Generic enough to be used in the pipe chain
  # for multiclass and binary curves
  maybe_group_by_level <- function(object, with_old = TRUE) {

    if (with_old) {
      grps <- dplyr::groups(object)
    } else {
      grps <- list()
    }

    if (".level" %in% colnames(object)) {
      # .level should be the first group b/c of how summarise()
      # drops a group
      object <- dplyr::group_by(object, .level, !!! grps)
    }
    object
  }

  # Construct poly_data
  # If grouped (ie resamples), we take the min of all "perfect" values
  #   to ensure we capture all lines in the polygon
  # If multiclass, we calculate each level separately
  poly_data <- object %>%
    maybe_group_by_level() %>%
    dplyr::summarise(slope = 1 / (max(.n_events) / dplyr::last(.n))) %>%
    dplyr::mutate(perfect = 100 / slope) %>%
    maybe_group_by_level(with_old = FALSE) %>%
    dplyr::summarise(perfect = min(perfect)) %>%
    maybe_group_by_level() %>%
    dplyr::do(
      dplyr::tibble(
        x = c(0, .$perfect, 100),
        y = c(0, 100, 100)
      )
    )

  # Avoid cran check for "globals"
  .percent_tested <- as.name(".percent_tested")
  .percent_found <- as.name(".percent_found")
  x <- as.name("x")
  y <- as.name("y")

  chart <- chart %+%

    # gain curve
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!.percent_tested,
        y = !!.percent_found,
        !!! interact_expr
      ),
      data = object
    ) %+%

    # boundary poly
    ggplot2::geom_polygon(
      mapping = ggplot2::aes(
        x = !!x,
        y = !!y
      ),
      data = poly_data,
      # fill
      fill = "lightgrey",
      alpha = 0.4
    ) %+%

    ggplot2::labs(
      x = "% Tested",
      y = "% Found"
    ) %+%

    ggplot2::theme_bw()

  # facet by .level if this was a multiclass computation
  if (".level" %in% colnames(object)) {
    chart <- chart %+%
      ggplot2::facet_wrap(~.level)
  }

  chart
}

# lift_df ----------------------------------------------------------------------

# dynamically exported in .onLoad()

#' @rdname gain_curve
autoplot.lift_df <- function(object, ...) {

  `%+%` <- ggplot2::`%+%`

  # Remove data before first event (is this okay?)
  object <- dplyr::filter(object, .n_events > 0)

  # Base chart
  chart <- ggplot2::ggplot(data = object)

  # Grouped specific chart features
  if (dplyr::is_grouped_df(object)) {

    # Construct the color interaction group
    grps <- dplyr::groups(object)
    interact_expr <- list(
      color = rlang::expr(interaction(!!! grps, sep = "_"))
    )

    # Add group legend label
    grps_chr <- paste0(dplyr::group_vars(object), collapse = "_")
    chart <- chart %+%
      ggplot2::labs(color = grps_chr)

  }
  else {
    interact_expr <- list()
  }

  baseline <- data.frame(
    x = c(0, 100),
    y = c(1, 1)
  )

  # Avoid cran check for "globals"
  .percent_tested <- as.name(".percent_tested")
  .lift <- as.name(".lift")
  x <- as.name("x")
  y <- as.name("y")

  chart <- chart %+%

    # gain curve
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!.percent_tested,
        y = !!.lift,
        !!! interact_expr
      ),
      data = object
    ) %+%

    # baseline
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        x = !!x,
        y = !!y
      ),
      data = baseline,
      colour = "grey60",
      linetype = 2
    ) %+%

    ggplot2::labs(
      x = "% Tested",
      y = "Lift"
    ) %+%

    ggplot2::theme_bw()

  # facet by .level if this was a multiclass computation
  if (".level" %in% colnames(object)) {
    chart <- chart %+%
      ggplot2::facet_wrap(~.level)
  }

  chart
}
