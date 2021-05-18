#' @title Compute combinations.
#' @description A function that computes the marker combinations and counts their corresponding positive samples for each class (once thresholds are selected).
#' A sample, to be considered positive for a given combination, must have a value higher than a given signal threshold (signalthr) for at least a given number of markers composing that combination (combithr).
#'@param data a data.frame returned by load_data().
#'@param signalthr a numeric that specifies the value above which a marker expression is considered positive in a given sample. Since the target of the analysis is the identification of marker combinations capable to correctly classify samples, the user should choose a signalthr that:

#' - Positively selects most samples belonging to the case class, which must be above signalthr.
#' - Negatively selects most control samples, which must be below signalthr.

#'@param combithr a numeric that specifies the necessary number of positivelly expressed markers (>= signalthr), in a given combination, to cosinder that combination positivelly expressed in a sample.
#'@return a data.frame containing how many samples of each class are "positive" for each combination.
#'@import gtools
#'@export


Combi <-function(data,signalthr=0, combithr=1){

  nclass <- unique(data$Class) # to retrieve the 2 classes


  #sample df to get names and dims
  dfe <- data[data$Class== nclass[1], 3:dim(data)[2]]
  dfe<-t(dfe)
  n_features<-length(rownames(dfe))

  markers <- as.factor(rownames(dfe))

  # parameters for combinations
  k<-1:n_features
  K<-2^n_features-1

  ### list of all possible combinations
  listCombinationMarkers <- array(0,dim=c(K,1))

  ### relative frequency for each class  (the row numbers depend on the K possible combinations while the column numbers depends on the number classes:2 (class A and class B, pairwise comparison))
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

  # creation of the dataframe with combinations and corresponding frequencies
  cdf <- data.frame(listCombinationMarkers, frequencyCombinationMarkers)
  colnames(cdf) <- c('Markers', paste('#Positives ', nclass[1]), paste('#Positives ', nclass[2]))
  for (i in 1:n_features){
    rownames(cdf)[i] <- cdf[i,1]
  }
  for (j in (n_features+1):length(rownames(cdf))){
    rownames(cdf)[j] <- paste('Combination', as.character(j-n_features) )
  }

  return(cdf)}
