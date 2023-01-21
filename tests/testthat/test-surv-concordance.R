test_that("comparison test with survival", {
  expected_res <- concordance_survival(
    data = lung_surv, truth = surv_obj, estimate = age
  )

  expect_equal(
    expected_res[[".estimate"]],
    survival::concordance(surv_obj ~ age, data = lung_surv)$concordance
  )
})
