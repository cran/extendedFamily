context("Testing heart data")
library(extendedFamily)

################
# Test loglog link
################

data(heart)

test_that("Confirm data can be loaded", {
  expect_true(nrow(heart) == 4483)
  expect_true(ncol(heart) == 11)
})
