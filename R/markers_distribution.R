
#' @title  Show distribution of intensity values for all the markers taken together.


#' @description A function that takes as input data in long format, and returns a named list containing the following objects:

#'  -  “Plot_density”: a density plot showing the distribution of the signal intensity values for both the classes.
#'  -  “ROC”: a ROC curve showing how many real positive samples would be found positive (SE) and how many real negative samples would be found negative (SP) in function of signal threshold. NB: these SE and SP are refereed to the signal intensity threshold considering all the markers together; it is NOT equal to the SE/SP of a single marker/combination found with SE_SP().
#'  - “Coord”: a dataframe that contains the coordinates of the above described “ROC” (threshold, SP and SE) that have at least a min SE (40 by default) and a min SP (80 by default).

#' In case of lack of a priori known threshold the user can set set signalthr_prediction= TRUE.
#' In this way the function provides a "suggested signal threshold" that corresponds to the median of the singnal threshold values (in "Coord") at which SE/SP are grater or equal to their set minimal values (min_SE and min_SP),
#' and it adds this threshold on the "Plot_density" object as a dashed black line.
#' The use of the median allows to pick a threshold whose SE/SP are not too close to the limits (min_SE and min_SP), but it is recommended to always inspect "Coord" and choose the most appropriate signal threshold by considering SP, SE and Youden index

#' @param data_long a data.frame in long format returned by CombiROC_long()
#' @param y_lim a numeric setting the max values of y that will be visualized in the density plot (zoom only, no data loss).
#' @param x_lim a numeric setting the max values of x that will be visualized in the density plot (zoom only, no data loss).
#' @param min_SE a numeric that specifies the min value of SE that a threshold must have to be included in $Coord.
#' @param min_SP a numeric that specifies the min value of SP that a threshold must have to be included in $Coord.
#' @param case_class a character that specifies which of the two classes of the dataset is the case class.
#' @param signalthr_prediction a boolean that specifies if the density plot will also show the "suggested signal threshold".
#' @return a named list containing 'Coord' data.frame, 'ROC' and 'Plot_density' plot objects.
#' @export


markers_distribution <- function(data_long, min_SE=40, min_SP=80, x_lim=NULL, y_lim=NULL , signalthr_prediction=FALSE, case_class) {

  if (min_SE==40 & min_SP==80){
    warning('In $Coord object you will see only the signal threshold values at which SE>=40 and SP>=80 by default. If you want to change this limits, please set min_SE and min_SP')
  }

  bin<- rep(NA, length(rownames(data_long)))
  for (i in 1:length(rownames(data_long))){
    if (data_long$Class[i] == case_class){bin[i] <- 1}
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
