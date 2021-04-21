source("~/Desktop/ivan/INGM/combiroc/combiroc_functions.R")
data<-load('data/demo_transcriptomics.csv', sep = ',')
data_long <- CombiROC_long(data)
markers_overview(data_long, ylim = 60)
tab <- Combi(data, signalthr = 8 )
mks <- SE_SP(data, tab)
rmks <- ranked_combs(data,mks, case_class ='Disease',min_SE = 40, min_SP =  80)
reports <- ROC_reports(data, tab, single_markers = 'One', selected_combinations = 15, case_class = 'Disease')
reports['Plot']
reports['Metrics']
reports['Models']
