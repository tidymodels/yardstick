context("Gain capture")

# Perfect ----------------------------------------------------------------------

# Perfect gain capture with .5 threshold
estimate <- c(.9, .8, .4, .68, .4)
truth <- factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No"))
df <- data.frame(truth, estimate)

test_that('Perfect gain capture', {
  expect_equal(
    gain_capture(df, truth, estimate)[[".estimate"]],
    1
  )
})

test_that('Antiperfect gain capture', {
  options(yardstick.event_first = FALSE)
  on.exit(options(yardstick.event_first = TRUE))
  expect_equal(
    gain_capture(df, truth, estimate)[[".estimate"]],
    -1
  )
})

# 1 Out of order ---------------------------------------------------------------

# 1 element out of order (3)
estimate2 <- c(.9, .8, .7, .68, .4)
truth2 <- factor(c("Yes", "Yes", "No", "Yes", "No"), levels = c("Yes", "No"))
df2 <- data.frame(truth2, estimate2)

# triangle + rectangle - .5 = shaded area
denom <- (3/5 * 1) / 2 + ((1 - 3/5) * 1) - .5

# triangle + rect + (triangle + rect) + rect - .5 = area under black line
# but above 45% line
numer <- (.4 * 2/3) / 2 +
         ( (.6 - .4) * 2/3) +
         ((.8 - .6) * 2/3) + ((.8 - .6) * (1-2/3)) / 2 +
         ((1-.8) * 1) -
         .5

test_that('1 out of order', {
  expect_equal(
    gain_capture(df2, truth2, estimate2)[[".estimate"]],
    numer / denom
  )
})

test_that('Anti 1 out of order', {
  options(yardstick.event_first = FALSE)
  on.exit(options(yardstick.event_first = TRUE))
  expect_equal(
    gain_capture(df2, truth2, estimate2)[[".estimate"]],
    - numer / denom
  )
})

# Multiclass ---------------------------------------------------------------

# macro, macro weighted?
