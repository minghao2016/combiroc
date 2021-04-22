# needed libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(gtools)
library(pROC)
library(stringr)

source("~/Desktop/ivan/INGM/combiroc/combiroc_functions.R")
#source("C:/Users/rossiriccardo/R/combiroc/combiroc_functions.R") 

data<-load('data/demo_transcriptomics.csv', sep = ',')
data_long <- CombiROC_long(data)
ov<- markers_overview(data_long, ylim = 60)
summ <- ov$Summary
ov$Plot
tab <- Combi(data, signalthr = 8 )
mks <- SE_SP(data, tab)
rmks <- ranked_combs(data,mks, case_class ='Disease',min_SE = 40, min_SP =  80)
reports <- ROC_reports(data, tab, single_markers = 'One', selected_combinations = 15, case_class = 'Disease')
reports['Plot']
reports['Metrics']
reports['Models']
show_markers(selected_combinations = 15, markers_table = tab)
combs_with(markers = c('Two', 'Three'), markers_table = tab)

