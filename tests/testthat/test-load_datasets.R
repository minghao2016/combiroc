test_that("A dataframe is read", {
  path <- file.path(system.file("data", package="combiroc"), "demo_data.csv")
  d <- load_data(data = path)
  expect_s3_class(d, "data.frame")
})

