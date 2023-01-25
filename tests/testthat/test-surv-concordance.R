test_that("comparison test with survival", {
  skip("waiting for concordance rewrite")
  expected_res <- concordance_survival(
    data = lung_surv, truth = surv_obj, estimate = age
  )

  expect_equal(
    expected_res[[".estimate"]],
    survival::concordance(surv_obj ~ age, data = lung_surv)$concordance
  )
})

test_that("case weights works", {
  skip("waiting for concordance rewrite")
  expected_res <- concordance_survival(
    data = lung_surv, truth = surv_obj, estimate = age, case_weights = ph.ecog
  )

  expect_equal(
    expected_res[[".estimate"]],
    survival::concordance(surv_obj ~ age, weights = ph.ecog, data = lung_surv)$concordance
  )
})
