# setup opjects for combiroc tests

data <- load_data(data = "demo_data.csv")
data_long <- combiroc_long(data)
distr <- markers_distribution(data_long, case_class = "A")
tab <- combi(data, signalthr = 450, combithr = 1)
mks <- se_sp(data, tab)
rmks <- ranked_combs(data, mks, case_class = 'A', min_SE = 40, min_SP = 80)
