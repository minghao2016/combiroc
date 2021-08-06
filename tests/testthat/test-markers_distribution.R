d <- load_data(data = "demo_data.csv")
d <- combiroc_long(d)
d <- markers_distribution(d, case_class = "A")

test_that("markers_ditribution generates a list of length 5", {
  expect_type(d, "list")
  expect_length(d, 5)
})

test_that("graphical obj objects are ggplot objects", {
  expect_s3_class(d$Boxplot, c("gg", "ggplot"))
  expect_s3_class(d$Density_plot, c("gg", "ggplot"))
  expect_s3_class(d$ROC, c("gg", "ggplot"))
})

test_that("table objects are data frames", {
  expect_s3_class(d$Coord, "data.frame")
  expect_s3_class(d$Density_summary, "data.frame")
})



