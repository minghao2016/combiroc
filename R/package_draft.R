### CombiROC package draft ###

## Dependencies:
library(tidyr)
library(dplyr)
library(ggplot2)

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
data_class_list <-c()
for (class in nclass) {data_class_list <- 
  list(data_class_list, dplyr::filter(data_long, Class == class))}

data_class_list[[1]]<- data_class_list[[1]][[2]] # to remove the first NULL element

names(data_class_list) <- nclass # to rename elements



# - a function to properly SELECT the INITIAL CUTOFF: it may return the box 
#   plot (MAD to get rid of outliers?) and a corresponding dataframe object 
#   (maybe with a message to aknowledge the user of its creation)


ggplot(data = data_long, aes(Markers, Values)) +
  geom_boxplot(aes(color = Class)) +
  theme_classic()  # ADD SUPERIOR LIMIT ?

for (class in nclass) {print(paste("STATISTICS OF CLASS ", class, ":", sep = ""))
                       print(summary(data.frame(data_class_list[class])[,4]))}

# - a function to perform the COMBINATORIAL ANALYSIS

# - a function to SHOW the BEST COMBINATIONS (dataframe + buble chart?):
#   ALTERNATIVE 1 - Do something like the app, where you select SN and SP 
#                   and the app returns the buble chart and the table
#   ALTERNATIVE 2 - The function returns the table with combinations 
#                   ranked by F1-score, showing on the top the 
#                   combinations with the highest SN and SP (I suppose)
#                   in order not to force a priori threshold selection.

# - a function to SHOW ROC CURVES and corresponding METRICS of the 
#   selected combinations

# - a function to ASSESS THE PERFORMANCES?