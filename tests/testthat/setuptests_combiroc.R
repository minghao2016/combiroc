# setup opjects for combiroc tests
data <- demo_data
data_long <- combiroc_long(data)
sms <- single_markers_statistics(data_long)
distr <- markers_distribution(data_long, case_class = "A")
tab <- combi(data, signalthr = 450, combithr = 1)
mks <- se_sp(data, tab)
rmks <- ranked_combs(data, mks, case_class = 'A', min_SE = 40, min_SP = 80)
reports <-roc_reports(data, markers_table = tab, case_class = 'A',
                      single_markers =c('Marker1'), selected_combinations = c(11,15))
sh_mk <- show_markers(selected_combinations =c(11,15), markers_table = tab)
combs_list <- combs_with(markers=c('Marker1', 'Marker3'), markers_table = tab)
unc_data <- demo_unclassified_data
cl_data <- classify(unc_data, Models =  reports$Models, Metrics = reports$Metrics, Positive_class = "affected", Negative_class = "healthy")
