test_that("An unclassified dataframe is read", {
  d <- load_unclassified_data(data = "demo_unclassified_data.csv", sep = ",")
  expect_s3_class(d, "data.frame")
})
