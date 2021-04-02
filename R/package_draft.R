### CombiROC package draft ###

## Dependencies:
library(tidyr)
library(dplyr)
library(ggplot2)
library(gtools)
library(ROCR)
library(stringr)

## Essential functions:

# - a function to READ DATA (if correctly formatted)

load <- function(data, sep = ";", na.strings="" ) {
  
CombiROC_data <- read.table(data, header = TRUE, sep = sep , na.strings=na.strings)

names(CombiROC_data)[2] <- ('Class')
cond_list <- rep(NA, dim(CombiROC_data)[2])
for (i in 1:dim(CombiROC_data)[2]){
cond_list[i] <- class(CombiROC_data[,i])=='numeric' | class(CombiROC_data[,i])=='integer'}

if (class(CombiROC_data[,1])!= 'character'){stop('Values of 1st column must be characters')}
else if (class(CombiROC_data[,2])!= 'character'){stop('Values of 2nd column must be characters')}
else if (length(unique(CombiROC_data[,2]))!=2){stop('2nd column must contain 2 categories (e.g. Disease / Healthy)')}
else if (sum(cond_list) != dim(CombiROC_data)[2]-2){stop('Values from 3rd column on must be numbers')}
else{return(CombiROC_data)}}





# - a function transform the data in long format for plotting purposes ?

## long format
CombiROC_long <- function(CombiROC_data){
  data_long <- tidyr::pivot_longer(data, cols =  3:dim(data)[2], names_to = "Markers", values_to = "Values")

  
  return(data_long)
}




# - a function to properly SELECT the INITIAL CUTOFF: it may return the box 
#   plot (MAD to get rid of outliers?) and a corresponding dataframe object 
#   (maybe with a message to aknowledge the user of its creation)

markers_overview <- function(data_long, ylim=NULL){
  
  nclass <- unique(data_long$Class)
  
  for (class in nclass){print(paste("STATISTICS OF CLASS ", class, ":", sep = ""))
    print(summary(data_long[data_long$Class==class, 4]))}
  
  ggplot(data_long, aes(Markers, Values)) +
    geom_boxplot(aes(color = Class)) +
    theme_classic()+
    coord_cartesian(ylim = c(0,ylim))}

# - a function to perform the COMBINATORIAL ANALYSIS

Combi <-function(data,signalthr=0, combithr=1){

nclass <- unique(data$Class)

#sample df to get names and dims
dfe <- data[data$Class== nclass[1], 3:dim(data)[2]]
dfe<-t(dfe)
n_features<-length(rownames(dfe))

# parameters for combinations
k<-1:n_features
K<-2^n_features-1   

### list of all possible combinations
listCombinationMarkers <- array(0,dim=c(K,1))

### relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
frequencyCombinationMarkers<-array(0,dim=c(K,2))

# COMPUTING COMBINATIONS AND FREQUENCIES
index<-1

for (i in 1:length(k)){
  temp<- combinations(n_features,k[i],rownames(dfe))
  # storing the row index to calculate the relative frequency
  row_index_combination<-combinations(n_features,k[i],k)
  for (j in 1:dim(temp)[1]){
    listCombinationMarkers[index,1]<-paste(temp[j,],collapse="-")
    ## single antigen
    if(dim(temp)[2]==1){ ## 1 antigen combination
      for (n in 1:length(nclass)){
         frequencyCombinationMarkers[index,n]<-length(which(
           t(data[data$Class== nclass[n], 3:dim(data)[2]])[row_index_combination[j,],]>=signalthr))    #input$signalthr
      }
    }else{ ## more than 1 antigen (combination)
      for (n in 1:length(nclass)){
         frequencyCombinationMarkers[index,n]<- length(which((colSums(
           t(data[data$Class== nclass[n], 3:dim(data)[2]])[row_index_combination[j,],]>=signalthr))>=combithr))   #input$signalthr))>=input$combithr
     }}
    index<-index+1
  }}

cdf <- data.frame(listCombinationMarkers, frequencyCombinationMarkers)
colnames(cdf) <- c('Markers', paste('#Positives ', nclass[1]), paste('#Positives ', nclass[2]))

for (i in 1:length(rownames(cdf))){
  rownames(cdf)[i] <- paste('Combination ', rownames(cdf)[i] )
}

return(cdf)}

# COMPUTING SE AND SP FOR EACH COMB. AND FOR BOTH CLASSES
SE_SP <- function(data, combinations_table){

mks <- combinations_table 
names<- c()
nclass <- unique(data$Class)
SE_SP<-array(0,dim=c(dim(mks)[1],2*2))

for (i in  1:length(nclass)){
  SE_SP[,i]<- round(mks[,i+1]*100/length(colnames(t(data[data$Class==nclass[i],])))
                      ,digits=0)
  names[i] <- paste('SE%', nclass[i])
  SE_SP[,i+2]<- 100-SE_SP[,i]
  names[i+2] <- paste('SP%', nclass[i])
}

SE_SP <- data.frame(SE_SP)
rownames(SE_SP)<-rownames(mks)
colnames(SE_SP)<- names
n_markers<- rep(NA, dim(mks)[1])

for (i in 1:dim(mks)[1]){
n_markers[i] <- str_count(mks$Markers[i], pattern = "-")+1}

SE_SP$count <- data.frame(n_markers)[,1]
colnames(SE_SP)[5] <- '# Markers' 

return(SE_SP)
}



#The function returns the table with combinations ranked by F1-score




ranked_combs <- function(data, combo_table, case_class) {

nclass <- unique(data$Class)

if (case_class == nclass[1]) {

combo_table$F1<-  2*(combo_table[,1] * combo_table[,4])/(combo_table[,1] + combo_table[,4])


ranked_SE_SP<-combo_table[order(-combo_table$F1), ]  # REMEMBER TO ADD FREQUENCY

ranked_SE_SP<- ranked_SE_SP[,c(1,4,5,6)]
return(ranked_SE_SP)}



else if (case_class == nclass[2]) {
  
  combo_table$F1<-  2*(combo_table[,2] * combo_table[,3])/(combo_table[,2] + combo_table[,3])
  
  
  ranked_SE_SP<-combo_table[order(-combo_table$F1), ]  # REMEMBER TO ADD FREQUENCY
  
  ranked_SE_SP<- ranked_SE_SP[,c(2,3,5,6)]
  return(ranked_SE_SP)}

else {
  stop('Please, specify the "case class" choosing from the 2 classes of your dataset')
}}


# - a function to SHOW ROC CURVES and corresponding METRICS of the 
#   selected combinations

# - a function to ASSESS THE PERFORMANCES?






### WORKFLOW TEST ###

data <- load("data/demo_5Ags.csv", sep=';')
data_long <- CombiROC_long(data)
markers_overview(data_long, ylim =2000)
mks <-Combi(data, signalthr = 450, combithr = 1)
rmks<- ranked_combs(data, mks, case_class = 'A')


