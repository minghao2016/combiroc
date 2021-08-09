# setup opjects for combiroc tests

data <- load_data(data = "demo_data.csv")
data_long <- combiroc_long(data)
distr <- markers_distribution(data_long, case_class = "A")
