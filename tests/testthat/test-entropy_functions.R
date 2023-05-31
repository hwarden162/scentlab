test_that("cut_entropy works", {

  counts <- matrix(1:12, ncol = 3)
  actual <- cut_entropy(counts = counts, threshold = 1.3)
  expected <- c(TRUE, FALSE, FALSE)

  expect_equal(actual, expected)

  expect_error(
    cut_entropy()
  )

})
