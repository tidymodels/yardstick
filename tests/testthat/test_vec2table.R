library(testthat)
library(yardstick)

test_that('bad args', {
  expect_warning(
    yardstick:::vec2table(truth = as.character(iris$Species), iris$Species)
  )
  expect_warning(
    yardstick:::vec2table(truth = iris$Species, as.character(iris$Species))
  )  
  expect_error(
    yardstick:::vec2table(
      truth = iris$Species, 
      rep(c("setosa", "virginica"), each = 75)
    )
  )  
  expect_error(
    yardstick:::vec2table(
      truth = iris$Species, 
      factor(rep(NA, 150), levels = levels(iris$Species)),
      na.rm = TRUE
    )
  )   
  expect_error(
    yardstick:::vec2table(
      truth = iris$Species, 
      factor(rep(letters[1:2], each = 75))
    )
  )  
  expect_error(
    yardstick:::vec2table(
      truth = iris$Species, 
      factor(rep(letters[1:3], each = 50))
    )
  )  
  expect_error(
    yardstick:::vec2table(
      truth = iris$Species, 
      factor(as.character(iris$Species), levels = rev(levels(iris$Species)))
    )
  ) 
  expect_error(
    yardstick:::vec2table(
      truth = factor(rep(letters[1], each = 50)), 
      factor(rep(letters[1], each = 50))
    )
  )  
})
