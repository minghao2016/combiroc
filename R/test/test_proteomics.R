# needed libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(gtools)
library(pROC)
library(stringr)

source("~/Desktop/ivan/INGM/combiroc/combiroc_functions.R")
#source("C:/Users/rossiriccardo/R/combiroc/combiroc_functions.R") 

data<-load('data/demo_5Ags.csv', sep = ';')
data_long <- CombiROC_long(data)
ov<- markers_overview(data_long, ylim = 1500)
summ <- ov$Summary
ov$Plot
tab <- Combi(data, signalthr = 450 )
mks <- SE_SP(data, tab)
rmks <- ranked_combs(data,mks, case_class ='A',min_SE = 40, min_SP =  80)
reports <- ROC_reports(data, tab, single_markers = 'Marker1', selected_combinations = c(1,15), case_class = 'A')
reports['Plot']
reports['Metrics']
reports['Models']
show_markers(selected_combinations = c(1,15), markers_table = tab)
combs_with(markers=c('Marker1', 'Marker3'), markers_table = tab)

unc_data <- load_unclassified_data(data = 'data/unclassified_proteomic_data.csv', sep = ',')
cl_data <- Classify(unc_data, Models =  reports$Models , Metrics = reports$Metrics)
