

load_data <- function(data, sep = ";", na.strings="" ) {

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


markers_distribution <- function(data_long, min_SE=40, min_SP=80, x_lim=NULL, y_lim=NULL , signalthr_prediction=FALSE, case_class) {

  if (min_SE==40 & min_SP==80){
    warning('In $Coord object you will see only the signal threshold values at which SE>=40 and SP>=80 by default. If you want to change this limits, please set min_SE and min_SP')
  }

  bin<- rep(NA, length(rownames(data_long)))
  for (i in 1:length(rownames(data_long))){
    if (data$Class[i] == case_class){bin[i] <- 1}
    else{bin[i] <- 0}}
  bin <- factor(bin)


  rocobj <-roc(data_long$Values, response=bin, levels=c("0","1"), quiet= TRUE)
  coord <- coords(rocobj)
  coord$Youden <- coord$specificity+coord$sensitivity
  coord$specificity <- round(coord$specificity*100)
  coord$sensitivity <- round(coord$sensitivity*100)
  coord <- coord[coord$specificity>=min_SP & coord$sensitivity>=min_SE, ]


  if (length(coord$threshold)==0){
    stop(' $Coord object is empty! No signal thresholds contained with SE >= min_SE  AND SP >= min_SP.')}


  if (is.null(x_lim)&is.null(y_lim)) {
    warning('You can adjust density plot zoom by setting y_lim and x_lim')
    p<- ggplot(data_long, aes(x=Values, color=Class)) +
      geom_density(n=10000) +
      theme_classic() }

  else if (is.null(x_lim)&!is.null(y_lim)) {
    p<- ggplot(data_long, aes(x=Values, color=Class)) +
      geom_density(n=10000) +
      theme_classic()+
      coord_cartesian(ylim = c(0, y_lim))}

  else if (!is.null(x_lim)&is.null(y_lim)) {
    p<- ggplot(data_long, aes(x=Values, color=Class)) +
      geom_density(n=10000) +
      theme_classic()+
      coord_cartesian(xlim = c(0, x_lim))}

  else  {
    p<- ggplot(data_long, aes(x=Values, color=Class)) +
      geom_density(n=10000) +
      theme_classic()+
      coord_cartesian(xlim = c(0, x_lim), ylim = c(0, y_lim))}

  if (isFALSE(signalthr_prediction)){
    res <- p+labs(x = "Signnal intensity", y="Frequency")
  }



  if (isTRUE(signalthr_prediction)){



   pr <- median(coord$threshold)
   warning('The suggested signal threshold in $Plot_density is the median of the signal thresholds at which SE>=min_SE and SP>=min_SP. This is ONLY a suggestion. Please check if signal threshold is suggested by your analysis kit guidelines instead, and remember to check $Plot_density to better judge our suggested threshold by inspecting the 2 distributions.')

    res <- p+geom_vline(aes(xintercept=pr),
                        color="black", linetype="dashed", size=0.5)+
     annotate("text", x = pr*0.60, y = 0, label =  as.character(round(pr)))+
      labs(x = "Signal intensity", y="Frequency")}

  robj <- list(res, coord, ggroc(rocobj))
  names(robj) <- c('Plot_density', 'Coord', 'ROC')
  return(robj)}

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
    combo_table$Youden<-  combo_table[,1] + combo_table[,4]
    combo_table$Youden<-combo_table$Youden/100
    ranked_SE_SP<-combo_table[order(-combo_table$Youden), ]
    ranked_SE_SP<- ranked_SE_SP[,c(1,4,5,6)]
    return(ranked_SE_SP[ranked_SE_SP[,1]>=min_SE & ranked_SE_SP[,2]>=min_SP,])}



  # if case class is the second
  else if (case_class == nclass[2]) {

    # computing F1 as 2*(SE 2nd class * SP 1st class) /  (SE 2nd class + SP 1st class)
    combo_table$Youden<-combo_table[,2] + combo_table[,3]
    combo_table$Youden<-combo_table$Youden/100
    ranked_SE_SP<-combo_table[order(-combo_table$Youden), ]
    ranked_SE_SP<- ranked_SE_SP[,c(2,3,5,6)]
    return(ranked_SE_SP[ranked_SE_SP[,1]>=min_SE & ranked_SE_SP[,2]>=min_SP,])}

  else {
    stop('Please, specify the "case class" choosing from the 2 classes of your dataset')
  }}


# - a function to plot ROC CURVES and retrieve ROC METRICS of the selected combinations

ROC_reports <- function(data, markers_table, selected_combinations=NULL, single_markers=NULL, case_class){
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
    roc_list[[which(sc==i)]]<-roc(data$Class,model_list[[which(sc==i)]]$fitted.values,levels=c("0","1"), quiet= TRUE)
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


# a function to load unclassified data


load_unclassified_data <- function(data, sep = ";", na.strings="" ) {

  unclassified_data <- read.table(data, header = TRUE, sep = sep ,
                              na.strings=na.strings)  # to load the data

  cond_list <- rep(NA, dim(unclassified_data)[2]) # to initialize a list of
  # conditions to check for columns with expression values
  cond_list1 <- rep(NA, dim(unclassified_data)[2]) # to initialize a list of
  # conditions to check for columns with expression values

  # checking the format ...
  for (i in 1:dim(unclassified_data)[2]){
    cond_list[i] <- class(unclassified_data[,i])=='numeric' | class(unclassified_data[,i])=='integer'
    cond_list1[i] <- str_detect(colnames(unclassified_data)[i],"-")}
  # True if a column contains numbers

  if (class(unclassified_data[,1])!= 'character'){stop('Values of 1st column must be characters')}
  # fist column must have patients/samples ID as characters



  else if (sum(cond_list) != dim(unclassified_data)[2]-1){stop('Values from 2nd column on must be numbers')}
  # number of numeric columns must be total number of columns -1

  else if (sum(cond_list1) >= 1){stop('"-" is not allowed in column names')}

  else{ # if it's ok
    # reordering marker columns alphabetically - necessary to properly compute combinations later
    d <- unclassified_data[, 2:dim(unclassified_data)[2]]
    d <- d[, order(colnames(d))]
    unclassified_data[,2:dim(unclassified_data)[2]] <- d

    colnames(unclassified_data)[2:dim(unclassified_data)[2]] <- colnames(d)
    return(unclassified_data)}}




# a function to fit the trained models on a new dataset to be classified


Classify <- function(unclassified_data, Models, Metrics, Positive_class=1, Negative_class=0){

classification <- list()

for (i in names(Models)){



  pred <- predict(Models[[i]], newdata = unclassified_data,
                  type = "response")
  cutoff <- Metrics[which(rownames(Metrics)==i), 4]
  pr_df <- data.frame(unclassified_data[,1])
  pr_df$predicted_class <- pred>cutoff
  colnames(pr_df)[1]<- colnames(unclassified_data)[1]
  pr_df$predicted_class[which(pr_df$predicted_class=='TRUE')] <- Positive_class
  pr_df$predicted_class[which(pr_df$predicted_class=='FALSE')] <- Negative_class
  classification[[i]] <- pr_df
}
return(classification)}
