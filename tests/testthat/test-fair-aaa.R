test_that("basic functionality", {
  data("hpc_cv")

  silly <- function(...) {
    metric_tibbler("a", "b", 1)
  }
  silly_metric <- new_class_metric(silly, "minimize")
  silly_new_groupwise_metric <-
    new_groupwise_metric(
      silly_metric,
      "silly",
      aggregate = function(x, ...) {
        x$.estimate[1]
      }
    )

  expect_true(inherits(silly_new_groupwise_metric, "function"))

  silly_fairness_Resample <- silly_new_groupwise_metric(Resample)
  expect_equal(attr(silly_fairness_Resample, "by"), "Resample")
  expect_s3_class(silly_fairness_Resample, "class_metric")

  expect_silent(
    m_set <- metric_set(silly_fairness_Resample)
  )

  expect_s3_class(m_set, "class_prob_metric_set")
  expect_equal(
    as_tibble(m_set),
    tibble::tibble(
      metric = "silly_fairness_Resample",
      class = "class_metric",
      direction = "minimize"
    )
  )

  expect_equal(
    m_set(hpc_cv, truth = obs, estimate = pred),
    tibble::tibble(
      .metric = "silly",
      .by = "Resample",
      .estimator = "b",
      ".estimate" = 1
    )
  )
})

test_that("new_groupwise_metric() works with grouped input", {
  skip_if_not_installed("tidyr")

  data("hpc_cv")
  hpc_cv$group <- sample(c("a", "b"), nrow(hpc_cv), replace = TRUE)

  expect_silent(
    m_set <-
      metric_set(
        equal_opportunity(group)
      )
  )

  grouped_res <-
    hpc_cv |>
    dplyr::group_by(Resample) |>
    m_set(hpc_cv, truth = obs, estimate = pred)

  hpc_split <- vctrs::vec_split(hpc_cv, hpc_cv$Resample)

  split_res <- dplyr::tibble(
    Resample = hpc_split$key,
    res = lapply(
      hpc_split$val,
      function(x) m_set(x, truth = obs, estimate = pred)
    )
  ) |>
    tidyr::unnest(cols = res)

  expect_identical(grouped_res, split_res)
})

test_that("metric factory print method works", {
  expect_snapshot(equal_opportunity)
})

test_that("can accommodate redundant sensitive features", {
  data("hpc_cv")

  expect_silent(
    m_set <-
      metric_set(
        demographic_parity(Resample),
        equal_opportunity(Resample)
      )
  )

  expect_s3_class(m_set, "class_prob_metric_set")

  res <- m_set(hpc_cv, truth = obs, estimate = pred)

  expect_equal(res$.metric, c("demographic_parity", "equal_opportunity"))
  expect_equal(res$.by, c("Resample", "Resample"))
})

test_that("can accommodate multiple sensitive features", {
  data("hpc_cv")

  hpc_cv$group <- sample(c("a", "b"), nrow(hpc_cv), replace = TRUE)

  expect_silent(
    m_set <-
      metric_set(
        demographic_parity(Resample),
        equal_opportunity(group)
      )
  )

  expect_s3_class(m_set, "class_prob_metric_set")

  res <- m_set(hpc_cv, truth = obs, estimate = pred)

  expect_equal(res$.metric, c("demographic_parity", "equal_opportunity"))
  expect_equal(res$.by, c("Resample", "group"))
})

test_that("can mix fairness metrics with standard metrics", {
  data("hpc_cv")

  expect_silent(
    m_set <-
      metric_set(
        demographic_parity(Resample),
        sens
      )
  )

  expect_s3_class(m_set, "class_prob_metric_set")

  res <- m_set(hpc_cv, truth = obs, estimate = pred)

  expect_equal(res$.metric, c("demographic_parity", "sens"))
  expect_equal(res$.by, c("Resample", NA_character_))

  m_set_sens <- metric_set(sens)
  expect_equal(
    res[2, ] |> dplyr::select(-.by),
    m_set_sens(hpc_cv, truth = obs, estimate = pred)
  )
})

test_that("can handle metric set input as `fn`", {
  data("hpc_cv")

  expect_silent(
    fairness_mtrc <-
      new_groupwise_metric(
        metric_set(sens, spec),
        "min_sens_spec",
        diff_range
      )
  )

  expect_s3_class(fairness_mtrc(Resample), "class_metric")
  expect_equal(attr(fairness_mtrc(Resample), "by"), "Resample")
  expect_equal(attr(fairness_mtrc(Resample), "direction"), "minimize")

  res <- fairness_mtrc(Resample)(hpc_cv, truth = obs, estimate = pred)

  expect_equal(res$.metric, "min_sens_spec")
  expect_equal(res$.by, "Resample")
})

test_that("handles `direction` input", {
  expect_silent(
    fairness_mtrc <-
      new_groupwise_metric(
        sens,
        "inv_sens_diff",
        function(x) {
          1 / diff(range(x$.estimate))
        },
        direction = "maximize"
      )
  )

  expect_equal(attr(fairness_mtrc(Resample), "direction"), "maximize")

  expect_snapshot(
    error = TRUE,
    new_groupwise_metric(
      sens,
      "bad_direction",
      diff_range,
      direction = "boop"
    )
  )
})

test_that("errors informatively with bad input", {
  expect_snapshot(
    error = TRUE,
    new_groupwise_metric()
  )

  expect_snapshot(
    error = TRUE,
    new_groupwise_metric(sens)
  )

  expect_snapshot(
    error = TRUE,
    new_groupwise_metric(sens, "bop")
  )

  expect_snapshot(
    error = TRUE,
    new_groupwise_metric("boop", "bop", identity)
  )

  expect_snapshot(
    error = TRUE,
    new_groupwise_metric(identity, "bop", identity)
  )

  expect_snapshot(
    error = TRUE,
    new_groupwise_metric(sens, 1, identity)
  )

  expect_snapshot(
    error = TRUE,
    new_groupwise_metric(sens, "bop", "boop")
  )
})

test_that("outputted function errors informatively with bad input", {
  data("hpc_cv")

  bad_aggregate <- new_groupwise_metric(sens, "bop", identity)
  expect_snapshot(
    error = TRUE,
    bad_aggregate(Resample)(hpc_cv, truth = obs, estimate = pred)
  )

  bad_by <- new_groupwise_metric(sens, "bop", identity)(nonexistent_column)
  expect_snapshot(
    error = TRUE,
    bad_by(hpc_cv, truth = obs, estimate = pred)
  )

  bad_truth_metric_set <-
    new_groupwise_metric(metric_set(sens, spec), "bop", function(x) {
      1
    })(Resample)
  expect_snapshot(
    error = TRUE,
    bad_truth_metric_set(hpc_cv, truth = VF, estimate = pred)
  )

  bad_truth_metric <-
    new_groupwise_metric(sens, "bop", function(x) {
      1
    })(Resample)
  expect_snapshot(
    error = TRUE,
    bad_truth_metric(hpc_cv, truth = VF, estimate = pred)
  )
})

test_that("outputted function errors informatively with redundant grouping", {
  data("hpc_cv")

  expect_snapshot(
    error = TRUE,
    hpc_cv |>
      dplyr::group_by(Resample) |>
      demographic_parity(Resample)(truth = obs, estimate = pred)
  )

  dp_res <- demographic_parity(Resample)

  expect_snapshot(
    error = TRUE,
    hpc_cv |>
      dplyr::group_by(Resample) |>
      dp_res(truth = obs, estimate = pred)
  )
})
