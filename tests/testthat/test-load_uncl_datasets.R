test_that("An unclassified dataframe is read", {
  expect_s3_class(d, "data.frame")
})
