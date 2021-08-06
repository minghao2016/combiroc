test_that("A dataframe is read", {
  d <- load_data(data = "demo_data.csv")
  expect_s3_class(d, "data.frame")
})

