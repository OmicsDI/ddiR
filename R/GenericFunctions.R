
#' Returns a data frame from a DatasetDetail list
#'
#' @param DatasetList instance 
#' @param row.names optional row namesnt
#' @param optional optional
#' @export
#' 
dataset.to.dataframe <- function(object, row.names=NULL, optional=FALSE, ...){
  value <- list.to.data.frame(object)
  return(value)
}

#' Returns a data frame from a list of objects
#'
#' @param list.of.objects The object list
#' @return The objects list as a data frame
#' @export
#' 
list.to.data.frame <- function(list.of.objects) {
  for(index in 1: length(list.of.objects)){
    df <- rbind(df, as.data.frame(list.of.objects[[index]]))
  }
  df
}

