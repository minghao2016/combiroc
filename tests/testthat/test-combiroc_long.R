test_that("combi_long returns a tibble", {
  d <- load_data(data = "demo_data.csv")
  d <- combiroc_long(d)
  expect_s3_class(d, c("tbl_df", "tbl", "data.frame"))
})
