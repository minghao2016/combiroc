
#'@title Classify data.frames using glm(link='binomial') models.
#'@description A function that applies the previously calculated models to an unclassified dataset and classifies the samples.
#' The unclassified dataset to be classified should be loaded with load_unclassified_data() and MUST contain all the markers of the classified dataset used to train the models (the one loaded with load()).


#' @param unclassified_data a data.frame returned by load_unclassified_data().
#' @param Models a list of glm() objects returned by ROC_reports().
#' @param Metrics a list of data.frame objects containing ROC metrics, returned by ROC_reports().
#' @param Positive_class a numeric or a character that specifies the label of the samples that will be classified as positives
#' @param Negative_class a numeric or a character that specifies the label of the samples that will be classified as negatives
#' @return  a named list of data.frames, one for each marker/combination contained in the list of models, containg the predicted class for each sample
#' @importFrom stats formula glm median predict quantile sd
#' @export
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
