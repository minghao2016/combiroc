selm <- show_markers(selected_combinations =c(11,15), markers_table = tab)
combs <- combs_with(markers=c('Marker1', 'Marker3'), markers_table = tab)

test_that("show_markers generates a dataframe of length 2", {
  expect_s3_class(selm, "data.frame")
  expect_length(selm, 2)
})

test_that("combs_with generates a numerical vector", {
  expect_type(combs, "double")
})
