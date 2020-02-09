## Helper getter functions for DatasetDetail objects


# set generic get.dataset.name
setGeneric("get.dataset.name", function(x) standardGeneric("get.dataset.name"))

#' Returns the original dataset title
#'
#' @param object a DatasetDetail
#' @return the dataset title
#' @author Enrique Audain
#' @export
setMethod("get.dataset.name", "DatasetDetail", function(x) x@name)


# set generic get.description
setGeneric("get.description", function(x) standardGeneric("get.description"))

#' Returns the dataset description
#'
#' @param object a DatasetDetail
#' @return the dataset description
#' @author Enrique Audain
#' @export
setMethod("get.description", "DatasetDetail", function(x) x@description)


# set generic get.dataset.tissues
setGeneric("get.dataset.tissues", function(x) standardGeneric("get.dataset.tissues"))

#' Returns tissues information described in the dataset
#'
#' @param object a DatasetDetail
#' @return the tissue information
#' @author Enrique Audain
#' @export
setMethod("get.dataset.tissues", "DatasetDetail", function(x) {
    paste(as.character(x@tissues), collapse = ',')
})


# set generic get.dataset.omics
setGeneric("get.dataset.omics", function(x) standardGeneric("get.dataset.omics"))

#' Returns the type of omics experiment (e.g. Proteomics, Genomics)
#'
#' @param object a DatasetDetail
#' @return the omics
#' @author Enrique Audain
#' @export
setMethod("get.dataset.omics", "DatasetDetail", function(x) {
    paste(as.character(x@omicsType), collapse = ',')
})


# set generic get.dataset.link
setGeneric("get.dataset.link", function(x) standardGeneric("get.dataset.link"))

#' Returns the dataset full link
#'
#' @param object a DatasetDetail
#' @return the dataset link
#' @author Enrique Audain
#' @export
setMethod("get.dataset.link", "DatasetDetail", function(x) x@full.dataset.link)


# set generic get.publication.date
setGeneric("get.publication.date", function(x) standardGeneric("get.publication.date"))

#' Returns the dataset publication date
#'
#' @param object a DatasetDetail
#' @return the dataset publication date
#' @author Enrique Audain
#' @export
setMethod("get.publication.date", "DatasetDetail", function(x) x@publication.date)


# set generic get.publication.ids
setGeneric("get.publication.ids", function(x) standardGeneric("get.publication.ids"))

#' Returns the publications IDs associated to the dataset
#'
#' @param object a DatasetDetail
#' @return the associated publication IDs
#' @author Enrique Audain
#' @export
setMethod("get.publication.ids", "DatasetDetail", function(x) {
    paste(as.character(x@publication.ids), collapse = ',')
})


# set generic get.organisms
setGeneric("get.organisms", function(x) standardGeneric("get.organisms"))

#' Returns the organisms described in the dataset
#'
#' @param object a DatasetDetail
#' @return the organisms
#' @author Enrique Audain
#' @export
setMethod("get.organisms", "DatasetDetail", function(x) {
    paste(as.character(x@organisms), collapse = ',')
})


# set generic get.instruments
setGeneric("get.instruments", function(x) standardGeneric("get.instruments"))

#' Returns the instruments used to generate the dataset
#'
#' @param object a DatasetDetail
#' @return the instruments
#' @author Enrique Audain
#' @export
setMethod("get.instruments", "DatasetDetail", function(x) {
    paste(as.character(x@instruments), collapse = ',')
})


# set generic get.diseases
setGeneric("get.diseases", function(x) standardGeneric("get.diseases"))

#' Returns the diseases under study
#'
#' @param object a DatasetDetail
#' @return the diseases under study
#' @author Enrique Audain
#' @export
setMethod("get.diseases", "DatasetDetail", function(x) {
    paste(as.character(x@diseases), collapse = ',')
})

