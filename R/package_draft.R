### CombiROC package draft ###

## Dependencies:
library(tidyr)
library(dplyr)
library(ggplot2)
library(gtools)

## Essential functions:

# - a function to READ DATA (if correctly formatted)
load <- function(data,  header = TRUE, sep = ";", na.strings="" ) {
  CombiROC_data <- read.table(data, header = header, sep = sep , na.strings=na.strings)
  # add format check

  return(CombiROC_data)
}

data <- load("data/demo_5Ags.csv")


# - a function transform the data in long format for plotting purposes ?

## long format
data_long <- tidyr::pivot_longer(data, cols =  starts_with("Marker"), names_to = "Markers", values_to = "Values")

## list of long data tables (one for each class)
nclass <- unique(data[,2])
long_data_class_list <-c()
for (class in nclass) {long_data_class_list <- 
  list(long_data_class_list, dplyr::filter(data_long, Class == class))}

long_data_class_list[[1]]<- long_data_class_list[[1]][[2]] # to remove the first NULL element

names(long_data_class_list) <- nclass # to rename elements



# - a function to properly SELECT the INITIAL CUTOFF: it may return the box 
#   plot (MAD to get rid of outliers?) and a corresponding dataframe object 
#   (maybe with a message to aknowledge the user of its creation)


ggplot(data = data_long, aes(Markers, Values)) +
  geom_boxplot(aes(color = Class)) +
  theme_classic()  # ADD SUPERIOR LIMIT ?

for (class in nclass) {print(paste("STATISTICS OF CLASS ", class, ":", sep = ""))
                       print(summary(data.frame(long_data_class_list[class])[,4]))}

# - a function to perform the COMBINATORIAL ANALYSIS
dfe <- data[data$Class== nclass[1],]
dfe<- dfe[,3:dim( dfe)[2]]
dfe<-t(dfe)

n_features<-length(rownames(dfe))
k<-1:n_features
K<-2^n_features-1   



df_comb_list <- c()
for (i in nclass) { df <- data[data$Class== i,]
  df <- df[,3:dim( df)[2]]
  df <- t(df)
  df_comb_list <- list(df_comb_list, df)}
df_comb_list[[1]]<- df_comb_list[[1]][[2]] # to remove the first NULL element

names(df_comb_list) <- nclass # to rename elements



### list of all possible combinations
listCombinationAntigens <- array(0,dim=c(K,1))

### relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))
frequencyCombinationAntigens<-array(0,dim=c(K,2))



signalthr<- 450
combithr <- 1

index<-1



for (i in 1:length(k)){
  temp<- combinations(n_features,k[i],rownames(dfe))
  # storage the row index to calculate the relative frequency
  row_index_combination<-combinations(n_features,k[i],k)
  for (j in 1:dim(temp)[1]){
    listCombinationAntigens[index,1]<-paste(temp[j,],collapse="-")
    ## single antigen
    if(dim(temp)[2]==1){ ## 1 antigen combination
      for (n in 1:length(nclass)){
         frequencyCombinationAntigens[index,n]<-length(which(
           data.frame(df_comb_list[n])[row_index_combination[j,],]>=signalthr))    #input$signalthr
      }
    }else{ ## more than 1 antigen (combination)
      for (n in 1:length(nclass)){
         frequencyCombinationAntigens[index,n]<- length(which((colSums(
           data.frame(df_comb_list[n])[row_index_combination[j,],]>=signalthr))>=combithr))   #input$signalthr))>=input$combithr
     }}
    index<-index+1
  }
}


# - a function to SHOW the BEST COMBINATIONS (dataframe + buble chart?):


length(colnames((data.frame(df_comb_list[1]))))

# Sensitivity  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (classA2 and classB2, pairwise comparison))


names<- c()

SE_SP<-array(0,dim=c(K,length(nclass)*2))

for (i in  1:length(nclass)){
  SE_SP[,i*2-1]<- round(frequencyCombinationAntigens[,1]*100/length(colnames((data.frame(df_comb_list[i]))))
                      ,digits=0)
  names[i*2-1] <- paste('SE%', nclass[i])
  SE_SP[,i*2]<- 100-SE_SP[,i*2-1]
  names[i*2] <- paste('SP%', nclass[i])
}


rownames(SE_SP)<-listCombinationAntigens
colnames(SE_SP)<- names

#   ALTERNATIVE 1 - Do something like the app, where you select SN and SP 
#                   and the app returns the buble chart and the table
#   ALTERNATIVE 2 - The function returns the table with combinations 
#                   ranked by F1-score, showing on the top the 
#                   combinations with the highest SN and SP (I suppose)
#                   in order not to force a priori threshold selection.

# - a function to SHOW ROC CURVES and corresponding METRICS of the 
#   selected combinations

# - a function to ASSESS THE PERFORMANCES?