#' A customized read.table() function that checks the conformity of the dataset format, and only if all checks are passed, loads it.

#' The dataset to be analysed should be in text format, which can be comma, tab or semicolon separated:
  
#' - The 1st column must contain patient/sample IDs as characters.
#' - The 2nd column must contain the class to which each sample belongs.
#' - The classes must be exactly 2 and they must be written in character format. 
#' - From the 3rd column on, the dataset must contain numerical values that represent the signal corresponding to the markers abundance in each sample (marker-related columns). 
#' - Marker-related columns can be called 'Marker1, Marker2, Marker3, ...' or can be called directly with the gene/protein name, but "-" is not allowed in the column name. 

#' Only if all the checks are passed, it reorders alphabetically the marker-related columns depending on marker names (necessary for a proper computation of combinations), and it forces "Class" as 2nd column name.

#' @param data the name of the file which the data are to be read from. 
#' @param sep the field separator character. 
#' @param na.strings a character vector of strings which are to be interpreted as NA values. 
#' @return a data frame (data.frame) containing a representation of the data in the file. 
#' @export 

load <- function(data, sep = ";", na.strings="" ) {
  
  CombiROC_data <- read.table(data, header = TRUE, sep = sep ,
                              na.strings=na.strings)  # to load the data
  
  names(CombiROC_data)[2] <- ('Class')  #  to force the name of 2nd column as
  # 'Class'
  
  cond_list <- rep(NA, dim(CombiROC_data)[2]) # to initialize a list of 
  # conditions to check for columns with expression values 
  cond_list1 <- rep(NA, dim(CombiROC_data)[2]) # to initialize a list of 
  # conditions to check for columns with expression values 
  
  # checking the format ...
  for (i in 1:dim(CombiROC_data)[2]){
    cond_list[i] <- class(CombiROC_data[,i])=='numeric' | class(CombiROC_data[,i])=='integer'
    cond_list1[i] <- str_detect(colnames(CombiROC_data)[i],"-")}
  # True if a column contains numbers
  
  if (class(CombiROC_data[,1])!= 'character'){stop('Values of 1st column must be characters')}
  # fist column must have patients/samples ID as characters
  
  else if (class(CombiROC_data[,2])!= 'character'){stop('Values of 2nd column must be characters')}
  # second column must contain the class of the samples as characters 
  
  else if (length(unique(CombiROC_data[,2]))!=2){stop('2nd column must contain 2 categories (e.g. Disease / Healthy)')}
  # only 2 categories are allowed 
  
  else if (sum(cond_list) != dim(CombiROC_data)[2]-2){stop('Values from 3rd column on must be numbers')}
  # number of numeric columns must be total number of columns -2 
  
  else if (sum(cond_list1) >= 1){stop('"-" is not allowed in column names')}
  # number of numeric columns must be total number of columns -2 
  
  else{ # if it's ok
    # reordering marker columns alphabetically - necessary to properly compute combinations later
    d <- CombiROC_data[, 3:dim(CombiROC_data)[2]]
    d <- d[, order(colnames(d))]
    CombiROC_data[,3:dim(CombiROC_data)[2]] <- d
    
    colnames(CombiROC_data)[3:dim(CombiROC_data)[2]] <- colnames(d)
    return(CombiROC_data)}}



#' A function that simply wraps dyplr::pivot_longer() to reshape data in long format.
#' @param data a data.frame obtained returned by load().
#' @return a data.frame in long format
#' @export
## long format
CombiROC_long <- function(data){
  data_long <- tidyr::pivot_longer(data, cols =  3:dim(data)[2], names_to = "Markers", values_to = "Values")
  
  return(data_long)
}




#' A function that provides an overview of the expression of each marker in the two classes of the dataset. It prints a summary statistics for each class 
#' and it returns a boxplot, both meant to help the user in the further selection of a signal threshold.

#' @param data_long a data.frame in long format returned by CombiROC_long()
#' @param ylim a numeric setting the max values of y that will be visualized in the boxplot (zoom only, no data loss).
#' @export

markers_overview <- function(data_long, ylim=NULL){
  
  nclass <- unique(data_long$Class) # to retrieve the 2 classes
  
  for (class in nclass){print(paste("STATISTICS OF CLASS ", class, ":", sep = ""))
    print(summary(data_long[data_long$Class==class, 4]))} # prints the summary of the class
  
  ggplot(data_long, aes(Markers, Values)) +
    geom_boxplot(aes(color = Class)) +
    theme_classic()+
    coord_cartesian(ylim = c(0,ylim))} # shows the boxplot for both classes 


#' A function that computes the marker combinations and counts their corresponding positive samples for each class (once thresholds are selected).
#' A sample, to be considered positive for a given combination, must have a value higher than a given signal threshold (signalthr) for at least a given number of markers composing that combination (combithr).
#'@param data a data.frame obtained returned by load().
#'@param signalthr a numeric that specifies the value above which a marker expression is considered positive in a given sample. Since the target of the analysis is the identification of marker combinations capable to correctly classify samples, the user should choose a signalthr that:

#' - Positively selects most samples belonging to the case class, which must be above signalthr.
#' - Negatively selects most control samples, which must be below signalthr.  

#'@param combithr a numeric that specifies the necessary number of positivelly expressed markers (>= signalthr), in a given combination, to cosinder that combination positivelly expressed in a sample.   
#'@return a data.frame containing how many samples of each class are "positive" for each combination.
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
  for (i in 1:length(rownames(cdf))){
    rownames(cdf)[i] <- paste('Combination ', rownames(cdf)[i] )
  }
  
  return(cdf)}



#' A function to compute sensitivity and specificity of each combination for each class.
#' The SE of a given combination (capability to find real positives/cases) corresponds to the SE of the case class, while its SP (capability to exclude real negatives/controls) corresponds to the SP of the control class.
#'@param data a data.frame obtained returned by load().
#'@param combinations_table a data.frame containing how many samples of each class are "positive" for each combination (returned by Combi()).
#'@return data.frame with SE, SP and number of composing markers for each combination.
#'@export


SE_SP <- function(data, combinations_table){
  
  mks <- combinations_table 
  names<- c()
  nclass <- unique(data$Class) # to retrieve the 2 classes
  
  SE_SP<-array(0,dim=c(dim(mks)[1],2*2)) # empty array to be filled
  
  for (i in  1:length(nclass)){
    SE_SP[,i]<- round(mks[,i+1]*100/length(colnames(t(data[data$Class==nclass[i],])))
                      ,digits=0) # SE of the given class
    names[i] <- paste('SE%', nclass[i]) 
    SE_SP[,i+2]<- 100-SE_SP[,i] # SP of the given class
    names[i+2] <- paste('SP%', nclass[i])
  }
  
  SE_SP <- data.frame(SE_SP) # from array to dataframe 
  rownames(SE_SP)<-rownames(mks)
  colnames(SE_SP)<- names
  
  # to add the count of markers composing the combination
  n_markers<- rep(NA, dim(mks)[1])
  for (i in 1:dim(mks)[1]){
    n_markers[i] <- str_count(mks$Markers[i], pattern = "-")+1} 
  # the number of markers is equal to number of '-' +1  => '-' must be avoided in marker name
  
  SE_SP$count <- data.frame(n_markers)[,1]
  colnames(SE_SP)[5] <- '# Markers' 
  
  return(SE_SP)
}



#' A function to rank combinations by F1-score and select them if they have a min SE and/or SP.
#' This function is meant to help the user in finding the best combinations (in the first rows) and allows also (not mandatory) the SE/SP-dependent filtering of combinations.
#' @param data a data.frame obtained returned by load().
#' @param combo_table a data.frame with SE, SP and number of composing markers for each combination (returned by SE_SP()).
#' @param case_class a character that specifies which of the two classes of the dataset is the case class.
#' @param min_SE a numeric that specifies the min value of SE that a combination must have to be filtered-in. 
#' @param min_SP a numeric that specifies the min value of SP that a combination must have to be filtered-in. 
#' @return a data.frame with ranked combination, reporting: SE, SP, number of markers composing the combination and the F1-score.
#' @export
 
ranked_combs <- function(data, combo_table, case_class, min_SE=0, min_SP=0) {
  
  nclass <- unique(data$Class) # to retrieve the 2 classes
  
  # if case class is the first
  if (case_class == nclass[1]) {
    
    # computing F1 as 2*(SE 1st class * SP 2nd class) /  (SE 1st class + SP 2nd class)
    combo_table$F1<-  2*(combo_table[,1] * combo_table[,4])/(combo_table[,1] + combo_table[,4])
    ranked_SE_SP<-combo_table[order(-combo_table$F1), ]  
    ranked_SE_SP<- ranked_SE_SP[,c(1,4,5,6)]
    return(ranked_SE_SP[ranked_SE_SP[,1]>=min_SE & ranked_SE_SP[,2]>=min_SP,])}
  
  
  
  # if case class is the second
  else if (case_class == nclass[2]) {
    
    # computing F1 as 2*(SE 2nd class * SP 1st class) /  (SE 2nd class + SP 1st class)
    combo_table$F1<-  2*(combo_table[,2] * combo_table[,3])/(combo_table[,2] + combo_table[,3])
    ranked_SE_SP<-combo_table[order(-combo_table$F1), ]  
    ranked_SE_SP<- ranked_SE_SP[,c(2,3,5,6)]
    return(ranked_SE_SP[ranked_SE_SP[,1]>=min_SE & ranked_SE_SP[,2]>=min_SP,])}
  
  else {
    stop('Please, specify the "case class" choosing from the 2 classes of your dataset')
  }}


#' A function to compute General Linear Model (binomial) and the corresponding ROC curves for each selected combination. 
#' @param data a data.frame obtained returned by load().
#' @param markers_table a data.frame with ranked combination, reporting: SE, SP, number of markers composing the combination and the F1-score (returned by ranked_combs()).
#' @param selected_combinations a numeric vector that specifies the combinations of interest. 
#' @param case_class a character that specifies which of the two classes of the dataset is the case class.
#' @param direction a character that specifies in which direction to make the comparison.
#' #' Direction can be set (not mandatory) in order to specify if the number of cases is <= (‘<’) or >= (‘>’) the number of controls. Otherwise the direction will be automatically set (default= ‘auto’), defining the group in which the median is higher as case group (see pROC::roc() documentation).
#' @return a named list containing 3 objects:

#' - "Plot": a ggplot object with the ROC curves of the selected combinations.
#' - "Metrics": a dataframe with the metrics of the roc curves (AUC, opt. cutoff, etc ...).
#' - "Models": the list of models (glm() objects) that have been computed and then used to classify the samples (in which you can find the model equation for each selected combination).
#' 
#' @export

ROC_reports <- function(data, markers_table, selected_combinations, case_class,  direction = "auto"){
  # to binarize $Class 
  bin<- rep(NA, length(rownames(data)))
  for (i in 1:length(rownames(data))){
    if (data$Class[i] == case_class){bin[i] <- 1}
    else{bin[i] <- 0}} 
  bin <- factor(bin)
  data$Class <- bin
  
  
  roc_list <- list() # It will contain ROC objects
  model_list <- list()
  
  # 0s dataframe to be filled
  perfwhole <-  data.frame(matrix(0, ncol = 12, nrow = length(selected_combinations)))
  
  
  # for each combination
  for ( i in selected_combinations){
    m <-str_split(mks$Markers[i],"-") # extract single markers from combination
    # for each composing marker
    for (x in m){ 
      y <- paste("log(",x,"+1)",sep="")} # partial formula
    
    str <- paste(y, collapse = '+')
    fla <- formula(paste("Class ~",str)) # whole formula
    
    glm.combo<-glm(fla,data=data, family="binomial")  # apply the glm model
    model_list[[which(selected_combinations==i)]]<- glm(fla,data=data, family="binomial")
    names(model_list)[which(selected_combinations==i)] <- rownames(mks)[i]
    
    # storing the ROC object by naming it with the corresponding combination 
    roc_list[[which(selected_combinations==i)]]<-roc(data$Class,glm.combo$fitted.values,levels=c("0","1"), direction=direction, quiet = FALSE)
    names(roc_list)[which(selected_combinations==i)] <- rownames(mks)[i]
    roc_obj <-roc_list[[which(selected_combinations==i)]]
    
    # retrieving metrics
    optcoordinates<-coords(roc_obj, "best", ret=c("threshold", "specificity",  "sensitivity", "accuracy","tn", 
                                                  "tp", "fn", "fp", "npv", "ppv", 
                                                  "1-specificity","1-sensitivity", "1-accuracy", "1-npv", "1-ppv"))
    
    # rounding metrics and computing ERR
    AUC <- round(roc_obj$auc[1],3)
    ACC <- round(optcoordinates[[4]],3)
    ERR <- round((optcoordinates[[8]]+optcoordinates[[7]])/dim(data)[1],3)    #(FP+FN)/P+N
    TP  <- round(optcoordinates[[6]],3)
    FP  <- round(optcoordinates[[8]],3)
    TN  <- round(optcoordinates[[5]],3)
    FN  <- round(optcoordinates[[7]],3)
    PPV <- round(optcoordinates[[10]],3)
    NPV <- round(optcoordinates[[9]],3)
    
    # adding a row containing a combination metrics to perfwhole dataframe 
    perfwhole[which(selected_combinations==i),] <- cbind(round(optcoordinates[[1]],3),round(optcoordinates[[3]],3),round(optcoordinates[[2]],3),AUC,ACC,ERR,TP,FP,TN,FN,PPV,NPV)
    rownames(perfwhole)[which(selected_combinations==i)] <- rownames(mks)[i]
  }
  
  colnames(perfwhole)<-c("CutOff","SE","SP","AUC","ACC","ERR","TP","FP","TN","FN","PPV","NPV")
  p <- ggroc(roc_list)
  res<-list(p,data.frame(perfwhole), model_list)
  
  names(res) <- c('Plot', 'Metrics', 'Models')
  
  return(res)}

