

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



# - a function transform the data in long format for plotting purposes ?

## long format
CombiROC_long <- function(data){
  data_long <- tidyr::pivot_longer(data, cols =  3:dim(data)[2], names_to = "Markers", values_to = "Values")
  
  return(data_long)
}




# - a function to properly SELECT the INITIAL CUTOFF: it may return the box 
#   plot (MAD to get rid of outliers?) and a corresponding dataframe object 
#   (maybe with a message to aknowledge the user of its creation)

markers_overview <- function(data_long, ylim=NULL){
  
  res <- list()
  nclass <- unique(data_long$Class) # to retrieve the 2 classes
  df <- data.frame(matrix(0, nrow = 2, ncol= 8))
  rownames(df) <- nclass
  colnames(df) <- c('# observations', 'Min', 'Max','Median', 'Mean', '1st Q.',  '3rd Q.', 'SD')
  
  
  for (i in 1:2){
    df[i,1] <-  dim(unique(data_long[data_long$Class==nclass[i],1]))[1]
    df[i,2] <- min(data_long[data_long$Class==nclass[i], 4])
    df[i,3] <- max(data_long[data_long$Class==nclass[i], 4])
    df[i,4] <- median(data_long[data_long$Class==nclass[i], 4][[1]])
    df[i,5] <- mean(data_long[data_long$Class==nclass[i], 4][[1]])
    df[i,6] <- as.numeric(quantile(t(data_long[data_long$Class==nclass[i], 4]),0.25))
    df[i,7] <- as.numeric(quantile(t(data_long[data_long$Class==nclass[i], 4]),0.75))
    df[i,8] <- sd(data_long[data_long$Class==nclass[i], 4][[1]])
  } # prints the summary of the class
    
  if (is.null(ylim)){
    ylim= max(df$Max)*1.15
    warning('ylim is not set. Boxplot may be difficult to interpret due to outliers. You should set an appropriate ylim.')
  }
  
  plot<- ggplot(data_long, aes(Markers, Values)) +
    geom_boxplot(aes(color = Class)) +
    theme_classic()+
    coord_cartesian(ylim = c(0,ylim)) # shows the boxplot for both classes 

res[[1]] <- df
res[[2]] <- plot
names(res) <- c('Summary', 'Plot')

return(res)}
# - a function to perform the COMBINATORIAL ANALYSIS

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

# COMPUTING SE AND SP FOR EACH COMB. AND FOR BOTH CLASSES
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



#The function returns the table with combinations ranked by F1-score


ranked_combs <- function(data, combo_table, case_class, min_SE=0, min_SP=0) {
  
  nclass <- unique(data$Class) # to retrieve the 2 classes
  
  # if case class is the first
  if (case_class == nclass[1]) {
    
    # computing F1 as 2*(SE 1st class * SP 2nd class) /  (SE 1st class + SP 2nd class)
    combo_table$score<-  2*(combo_table[,1] * combo_table[,4])/(combo_table[,1] + combo_table[,4])
    ranked_SE_SP<-combo_table[order(-combo_table$score), ]  
    ranked_SE_SP<- ranked_SE_SP[,c(1,4,5,6)]
    return(ranked_SE_SP[ranked_SE_SP[,1]>=min_SE & ranked_SE_SP[,2]>=min_SP,])}
  
  
  
  # if case class is the second
  else if (case_class == nclass[2]) {
    
    # computing F1 as 2*(SE 2nd class * SP 1st class) /  (SE 2nd class + SP 1st class)
    combo_table$score <-  2*(combo_table[,2] * combo_table[,3])/(combo_table[,2] + combo_table[,3])
    ranked_SE_SP<-combo_table[order(-combo_table$score), ]  
    ranked_SE_SP<- ranked_SE_SP[,c(2,3,5,6)]
    return(ranked_SE_SP[ranked_SE_SP[,1]>=min_SE & ranked_SE_SP[,2]>=min_SP,])}
  
  else {
    stop('Please, specify the "case class" choosing from the 2 classes of your dataset')
  }}


# - a function to plot ROC CURVES and retrieve ROC METRICS of the selected combinations

ROC_reports <- function(data, markers_table, selected_combinations=NULL, single_markers=NULL, case_class,  direction = "auto"){
  # to binarize $Class 
  bin<- rep(NA, length(rownames(data)))
  for (i in 1:length(rownames(data))){
    if (data$Class[i] == case_class){bin[i] <- 1}
    else{bin[i] <- 0}} 
  bin <- factor(bin)
  data$Class <- bin
  
  tab <- markers_table
  
  roc_list <- list() # It will contain ROC objects
  model_list <- list()
  
  
  if (is.null(selected_combinations)){
    sc<- single_markers}
  if (!is.null(selected_combinations)){
  sc<- selected_combinations + (length(colnames(data))-2)}
 if (!is.null(single_markers)){
  for (i in 1:length(single_markers)){
  single_markers[i] <- which(rownames(mks)== single_markers[i])
  }
  sc <- as.numeric(union(single_markers,sc))
 }
  
  
  AUC <- rep(0, length(sc))
  
  # 0s dataframe to be filled
  perfwhole <-  data.frame(matrix(0, ncol = 10, nrow = length(sc)))

  
  # for each combination
  for ( i in sc){
    m <-str_split(tab$Markers[i],"-") # extract single markers from combination
    # for each composing marker
    for (x in m){ 
      y <- paste("log(",x,"+1)",sep="")} # partial formula
    
    str <- paste(y, collapse = '+')
    fla <- formula(paste("Class ~",str)) # whole formula
    
    model_list[[which(sc==i)]]<- glm(fla,data=data, family="binomial")
    names(model_list)[which(sc==i)] <- rownames(tab)[i]
    
    # storing the ROC object by naming it with the corresponding combination 
    roc_list[[which(sc==i)]]<-roc(data$Class,model_list[[which(sc==i)]]$fitted.values,levels=c("0","1"), direction=direction, quiet = FALSE)
    names(roc_list)[which(sc==i)] <- rownames(tab)[i]
    
    
    # retrieving metrics
    optcoordinates<-coords(roc_list[[which(sc==i)]], "best", ret=c("threshold", "specificity",  "sensitivity", "accuracy","tn", 
                                                  "tp", "fn", "fp", "npv", "ppv"))
    

    
    # adding a row containing a combination metrics to perfwhole dataframe 
    perfwhole[which(sc==i),] <- optcoordinates[1,]

    rownames(perfwhole)[which(sc==i)] <- rownames(tab)[i]
  
    AUC[which(sc==i)] <- roc_list[[which(sc==i)]]$auc[1]
    }
  colnames(perfwhole)<-c("CutOff","SP","SE","ACC","TN","TP","FN","FP","NPV","PPV")
  perfwhole <- mutate(perfwhole, AUC = AUC)
  perfwhole <- perfwhole[,c(11,3,2,1,4,5,6,7,8,9,10)]
  
  p <- ggroc(roc_list)
  res<-list(p,round(perfwhole,3), model_list)
  
  names(res) <- c('Plot', 'Metrics', 'Models')
  
  return(res)}


# to show the composition of combinations of interest

show_markers <- function(markers_table, selected_combinations){
 df<- data.frame(matrix(0, ncol=2, nrow=length(selected_combinations)))
     combo_list <- list()
     markers_list <- list()
     for (i in 1:length(selected_combinations)){
     combo_list[i]<- paste('Combination',  as.character(selected_combinations[i]))
     df[i,1] <- combo_list[[i]]
     markers_list[i] <- markers_table[which(rownames(markers_table)==combo_list[i]), 1]
     df[i,2] <- markers_list[[i]]
     }
    colnames(df) <- c('Combination', 'Composing markers') 
    
  return(df)}


# to find all the combinations containing all the markers of interest 
combs_with<- function(markers, markers_table){
  mask <- rep(NA,dim(markers_table)[1])
 
  for (i in 1:dim(markers_table)[1]){ 
  mask[i] <- sum(str_count(markers_table[i,1], pattern = markers))==length(markers)
  }
  rownames(markers_table[mask,])
  combs <- as.numeric(gsub("Combination", "", rownames(markers_table[mask,])))
  if (length(combs)==0){
    warning('NO COMBINATION FOUND! Please check the selected markers')
  }
  
  message('The combinations in which you can find ALL the selected markers have been computed')
  
  return(combs)}