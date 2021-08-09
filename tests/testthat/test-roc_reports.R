test_that("roc_reports generates a list of length 3", {
  expect_type(reports, "list")
  expect_length(reports, 3)
})

test_that("ROC plot in reports objects is a ggplot objects", {
  expect_s3_class(reports$Plot, c("gg", "ggplot"))
})

test_that("Metrics object is a data frame of dim 3 rows and 11 columns", {
  expect_s3_class(reports$Metrics, "data.frame")
  expect_length(reports$Metrics, 11)
  expect_length(t(reports$Metrics), 33)
})
