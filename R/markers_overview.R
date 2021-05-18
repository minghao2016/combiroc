#' @title Show an overview of the expression of each marker.
#' @description A function that provides an overview of the expression of each marker in the two classes of the dataset.
#' @details This function returns a named list that contains two objects:

#' - ‘Plot’: a boxplot whose y max value can be set, in order to allow a better visualization (zoom only, no data loss).
#' - ‘Summary’: a data.frame with a summary statics of the overall expression of markers in the two classes of the dataset. It  a summary statistics for each class.
#'
#' @param data_long a data.frame in long format returned by CombiROC_long()
#' @param ylim a numeric setting the max values of y that will be visualized in the boxplot (zoom only, no data loss).
#' @return a named list containing 'Summary' data.frame and 'Plot' object.
#' @import ggplot2
#' @export

markers_overview <- function(data_long, ylim=NULL){

Class <- data_long$Class
Markers <- data_long$Markers
Values <- data_long$Values


  res <- list()
  nclass <- unique(Class) # to retrieve the 2 classes
  df <- data.frame(matrix(0, nrow = 2, ncol= 8))
  rownames(df) <- nclass
  colnames(df) <- c('# observations', 'Min', 'Max','Median', 'Mean', '1st Q.',  '3rd Q.', 'SD')


  for (i in 1:2){
    df[i,1] <-  dim(unique(data_long[Class==nclass[i],1]))[1]
    df[i,2] <- min(data_long[Class==nclass[i], 4])
    df[i,3] <- max(data_long[Class==nclass[i], 4])
    df[i,4] <- median(data_long[Class==nclass[i], 4][[1]])
    df[i,5] <- mean(data_long[Class==nclass[i], 4][[1]])
    df[i,6] <- as.numeric(quantile(t(data_long[Class==nclass[i], 4]),0.25))
    df[i,7] <- as.numeric(quantile(t(data_long[Class==nclass[i], 4]),0.75))
    df[i,8] <- sd(data_long[Class==nclass[i], 4][[1]])
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

