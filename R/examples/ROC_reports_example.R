# To train logistic regression models on each selected combinations and
# each selected marker, and compute corresponding ROCs.

## reports <- ROC_reports(data= demo_data, markers_table= ranked_combs_SE_SP,
##                        selected_combinations= c(1,11,15),
##                        single_markers=c('Marker1', 'Marker2'), case_class='A')
##
## reports$Plot  # Shows the ROC curves
## reports$Metrics # Shows the ROC metrics
## report$Models # show models
## report$reports$Models$`Combination 11` # show model trained with Combination 1
