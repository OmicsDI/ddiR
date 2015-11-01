#' Returns a data frame from a list of objects
#'
#' @param list.of.objects The object list
#' @return The objects list as a data frame
#' @author Yasset Perez-Riverol
#' @details This function convert an R frame from a list of objects.
#' @export

list.to.data.frame <- function(list.of.objects) {
        do.call(rbind.data.frame, lapply(list.of.objects, as.data.frame))
    }
