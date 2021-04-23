# needed libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(gtools)
library(pROC)
library(stringr)

source("~/Desktop/ivan/INGM/combiroc/combiroc_functions.R")
#source("C:/Users/rossiriccardo/R/combiroc/combiroc_functions.R") 

data<-load(, sep = )
data_long <- CombiROC_long(data)
ov<- markers_overview(data_long, ylim = )
summ <- ov$Summary
ov$Plot
tab <- Combi(data, signalthr = , combithr =  )
mks <- SE_SP(data, tab)
rmks <- ranked_combs(data,mks, case_class =, min_SE = , min_SP =  )
reports <- ROC_reports(data, tab, single_markers = , selected_combinations = , case_class = )
reports['Plot']
reports['Metrics']
reports['Models']
show_markers(selected_combinations = , markers_table = tab)
combs_with(markers = , markers_table = tab)

