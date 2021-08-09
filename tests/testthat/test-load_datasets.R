d <- load_data(data = "demo_data.csv")

test_that("A dataframe is read", {
  expect_s3_class(d, "data.frame")
})

test_that("the name of second column is enforced to Class", {
  expect_setequal(colnames(d)[2], "Class")
})
