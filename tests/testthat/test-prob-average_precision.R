context("test-prob-average_precision")

test_that("known corner cases are correct", {

  # first value - tp = 1
  truth <- factor("a", levels = c("a", "b"))
  estimate <- .9
  df <- data.frame(truth, estimate)

  expect_equal(
    average_precision(df, truth, estimate)$.estimate,
    1
  )

  # With the recall == 0 case precision value
  # defined to be precision == 1, we also expect
  # these to match pr_auc()

  expect_equal(
    average_precision(df, truth, estimate)$.estimate,
    pr_auc(df, truth, estimate)$.estimate
  )

  # first value - fp = 1, no `truth` events
  truth <- factor("b", levels = c("a", "b"))
  estimate <- .9
  df <- data.frame(truth, estimate)

  expect_equal(
    expect_warning(
      average_precision(df, truth, estimate)$.estimate,
      "There are `0` event cases"
    ),
    NA_real_
  )

  # Same as pr_auc()
  expect_equal(
    expect_warning(
      average_precision(df, truth, estimate)$.estimate,
      "There are `0` event cases"
    ),
    expect_warning(
      pr_auc(df, truth, estimate)$.estimate,
      "There are `0` event cases"
    )
  )

})

test_that("`event_level = 'second'` works", {
  df <- two_class_example

  df_rev <- df
  df_rev$truth <- relevel(df_rev$truth, "Class2")

  expect_equal(
    average_precision_vec(df$truth, df$Class1),
    average_precision_vec(df_rev$truth, df_rev$Class1, event_level = "second")
  )
})
