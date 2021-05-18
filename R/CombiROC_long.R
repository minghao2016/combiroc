
#' @title Reshape CombiROC data in long format.
#' @description A function that simply wraps dyplr::pivot_longer() to reshape data in long format.
#' @param data a data.frame returned by load_data().
#' @return a data.frame in long format
#' @export

CombiROC_long <- function(data){
  data_long <- tidyr::pivot_longer(data, cols =  3:dim(data)[2], names_to = "Markers", values_to = "Values")

  return(data_long)
}

