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
#' @return list of the tissues information
#' @author Enrique Audain
#' @export
setMethod("get.dataset.tissues", "DatasetDetail", function(x) {
    unlist(lapply(x@tissues, function(x) x))
})


# set generic get.dataset.omics
setGeneric("get.dataset.omics", function(x) standardGeneric("get.dataset.omics"))

#' Returns the type of omics experiment (e.g. Proteomics, Genomics)
#'
#' @param object a DatasetDetail
#' @return list of omics type described in the study
#' @author Enrique Audain
#' @export
setMethod("get.dataset.omics", "DatasetDetail", function(x) {
    unlist(lapply(x@omicsType, function(x) x))
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
#' @return list of associated publication IDs
#' @author Enrique Audain
#' @export
setMethod("get.publication.ids", "DatasetDetail", function(x) {
    unlist(lapply(x@publication.ids, function(x) x))
})


# set generic get.organisms
setGeneric("get.organisms", function(x) standardGeneric("get.organisms"))

#' Returns the organisms described in the dataset
#'
#' @param object a DatasetDetail
#' @return Named list (accession:organism) of organisms described in the study
#' @author Enrique Audain
#' @export
setMethod("get.organisms", "DatasetDetail", function(x) {
    v <- vector()
    for(organism in x@organisms){
        v[[organism@accession]] <- organism@name
    }
    return(v)
})


# set generic get.instruments
setGeneric("get.instruments", function(x) standardGeneric("get.instruments"))

#' Returns the instruments used to generate the dataset
#'
#' @param object a DatasetDetail
#' @return list of the instruments used in the study
#' @author Enrique Audain
#' @export
setMethod("get.instruments", "DatasetDetail", function(x) {
    unlist(lapply(x@instruments, function(x) x))
})


# set generic get.diseases
setGeneric("get.diseases", function(x) standardGeneric("get.diseases"))

#' Returns the diseases under study
#'
#' @param object a DatasetDetail
#' @return list of the diseases under study
#' @author Enrique Audain
#' @export
setMethod("get.diseases", "DatasetDetail", function(x) {
    unlist(lapply(x@diseases, function(x) x))
})


# set generic get.keywords
setGeneric("get.keywords", function(x) standardGeneric("get.keywords"))

#' Returns the keywords described in the study
#'
#' @param object a DatasetDetail
#' @return list of study keywords
#' @author Enrique Audain
#' @export
setMethod("get.keywords", "DatasetDetail", function(x) {
    unlist(lapply(x@keywords, function(x) x))
})


# set generic get.experiments.type
setGeneric("get.experiments.type", function(x) standardGeneric("get.experiments.type"))

#' Returns the experiments type described in the study
#'
#' @param object a DatasetDetail
#' @return list of experiments type
#' @author Enrique Audain
#' @export
setMethod("get.experiments.type", "DatasetDetail", function(x) {
    unlist(lapply(x@experiment.type, function(x) x))
})
