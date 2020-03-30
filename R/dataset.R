ddi_url     <- "http://www.omicsdi.org/ws"
MISSING_VALUE <- "Not available"

setMethod("show",
          signature = "DatasetSummary",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep="")
              cat("    Title: ", object@title, "\n", sep="")
              cat("    Dataset Id: ", object@dataset.id, "\n", sep="")
              cat("    Database: ", object@database, "\n", sep="")
              cat("    Description: ", object@description, "\n", sep="")
              cat("    Publication Date #: ", object@publication.date, "\n", sep="")
              cat("    keywords # ", paste0(object@keywords, collapse = '\t'), "\n", sep="")
              cat("    organisms #: ", paste0(object@organisms, collapse = '\t'), "\n", sep="")
              cat("    Count Visit #: ", object@visit.count, "\n", sep="")
              invisible(NULL)
          }
)

if (!isGeneric("dataset.id")) {
    setGeneric("dataset.id", function(object) standardGeneric("dataset.id"))
}

#' Returns an acession id
#'
#' @param object a DatasetSummary
#' @return the accession
#' @author Yasset Perez-Riverol
#' @export
setMethod("dataset.id", "DatasetSummary", function(object) object@dataset.id)

if (!isGeneric("database")) {
    setGeneric("database", function(object) standardGeneric("database"))
}

#' Returns an acession id
#'
#' @param object a DatasetSummary
#' @return database accession
#' @author Yasset Perez-Riverol
#' @export
setMethod("database", "DatasetSummary", function(object) object@database)


#' Returns an acession id
#'
#' @param object a DatasetDetail
#' @return the accession
#' @author Yasset Perez-Riverol
#' @export
setMethod("dataset.id", "DatasetDetail", function(object) object@dataset.id)


#' Returns an acession id
#'
#' @param object a DatasetDetail
#' @return database accession
#' @author Yasset Perez-Riverol
#' @export
setMethod("database", "DatasetDetail", function(object) object@database)


#' Returns a DatasetResult instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The DatasetResult instance
#' @author Yasset Perez-Riverol
#' @details TOD
#' @export
from.json.DataSetResult <- function(json.object) {
    localOrganisms <- list(MISSING_VALUE)
    if(!((is.null(json.object$organisms) || (length(json.object$organisms)==0)))){
        localOrganisms <- lapply(json.object$organisms,function (x) {from.json.Organism(x)})
    }
    res <- new("DatasetResult",
               title = json.object$title,
               dataset.id = json.object$id,
               database = json.object$source,
               description = json.object$description,
               publication.date = json.object$publicationDate,
               keywords = ifelse(is.null(json.object$keywords)||(length(json.object$keywords)==0),
                                 c(MISSING_VALUE), json.object$keywords),
               organisms = localOrganisms,
               visit.count = json.object$visitCount
    )
    return (res)
}


#' from.json.Organism
#'
#' This function converts an Organism json object to Organism
#'
#' @param json.object input json object
#' @return Organism
#'
from.json.Organism <- function(json.object){
    res <- new ("Organism",
                name = ifelse(is.null(json.object$name) || (length(json.object$name) == 0),MISSING_VALUE, json.object$name),
                accession = ifelse(is.null(json.object$acc) || (length(json.object$acc) == 0),MISSING_VALUE, json.object$acc)
    )
    return(res)
}

#' from.json.Protocol This function converts a json Protocol object to a Protocol object
#'
#' @param json.object This json object contain the information of a protocol
#' @return Protocol Object
#'
from.json.Protocol <- function(json.object){
    res <- new("Protocol",
               name  = ifelse(is.null(json.object$name) || (length(json.object$name) == 0),MISSING_VALUE, json.object$name),
               description = ifelse(is.null(json.object$description) || (length(json.object$description) == 0),MISSING_VALUE, json.object$description)
    )
    return(res)
}

#' from.json.LabMember
#'
#' This function convert objects from json to LabMember
#' @param json.object This json object contains the information of a LabMember
#' @return LabMember
#'
from.json.LabMember <- function(json.object){
    res <- new("LabMember",
               name = json.object$name,
               role = json.object$role,
               affilation = json.object$affilation,
               email = json.object$email)
    return(res)
}

#' from.json.DatasetDetail This function converts a json DatasetDetail to a DatasetDetail
#'
#'@param json.object a DatasetDetail json object
#'@return DatasetDetail
#'
from.json.DatasetDetail <- function(json.object){

    localProtocols <- c(MISSING_VALUE)
    if(!(is.null(json.object$protocols) || (length(json.object$protocols) == 0))){
        localProtocols <- lapply(json.object$protocols,function(x){
            from.json.Protocol(x)
        }
        )
    }
    localOrganisms <- c(MISSING_VALUE)
    if(!(is.null(json.object$organisms) || (length(json.object$organisms) == 0))){
        localOrganisms <- lapply(json.object$organisms,function(x){
            from.json.Organism(x)
        }
        )
    }
    localLabMembers <- c(MISSING_VALUE)
    if(!(is.null(json.object$labMembers) || (length(json.object$labMembers) == 0))){
        localLabMembers <- lapply(json.object$labMembers,function(x){
            from.json.LabMember(x)
        }
        )
    }


    fullLink <- c(MISSING_VALUE)
    if(!(is.null(json.object$full_dataset_link) || (length(json.object$full_dataset_link) == 0))){
        fullLink <- json.object$full_dataset_link
    }

    localTissues <- list(MISSING_VALUE)
    if(!(is.null(json.object$tissues) || (length(json.object$tissues) == 0))){
        localTissues <- json.object$tissues
    }

    localExperimentType <- list(MISSING_VALUE)
    if(!(is.null(json.object$experimentType))){
        localExperimentType <- json.object$experimentType
    }

    localInstruments <- list(MISSING_VALUE)
    if (!(is.null(json.object$instruments) || (length(json.object$instruments) == 0))){
        localInstruments <- json.object$instruments
    }

    localDiseases  <- list(MISSING_VALUE)
    if( !(is.null(json.object$diseases) || (length(json.object$diseases) == 0))){
        localDiseases <- json.object$diseases;
    }

    localKeywords <- list(MISSING_VALUE)
    if(!(is.null(json.object$keywords) || (length(json.object$keywords) == 0))){
        localKeywords <- json.object$keywords
    }

    localOmicsType <- list(MISSING_VALUE)

    if( !(is.null(json.object$omics_type) || (length(json.object$omics_type) == 0))){
        localOmicsType <- json.object$omics_type;
    }

    localPublicationIDs = list(MISSING_VALUE)
    if(!(is.null(json.object$publicationIds) || (length(json.object$publicationIds) == 0))){
        localPublicationIDs <- json.object$publicationIds;
    }

    localPublicationDate = c(MISSING_VALUE)
    if(!(is.null(json.object$publicationDate) || length(json.object$publicationDate) == 0)){
        localPublicationDate <- json.object$publicationDate
    }


    res <- new("DatasetDetail",
               name            = json.object$name,
               dataset.id      = json.object$id,
               description     = json.object$description,
               database        = json.object$source,
               publication.date = localPublicationDate,
               protocols       = localProtocols,
               keywords        = localKeywords,
               tissues         = localTissues,
               diseases        = localDiseases,
               instruments     = localInstruments,
               experiment.type = localExperimentType,
               publication.ids = localPublicationIDs,
               organisms       = localOrganisms,
               full.dataset.link = fullLink,
               omicsType       = localOmicsType,
               lab.members     = localLabMembers
    )
    return(res)
}


#' from.json.FacetValue
#'
#' This function converts a json FacetValue object to FaceTvalue
#' @param json.object This object is a facetValue json object
#' @return FacetValue a Final class with the object
#'
from.json.FacetValue <- function(json.object){
    res <- new("FacetValue",
               value = json.object$value,
               count = json.object$count,
               label = json.object$label
    )
    return(res)
}



#' from.json.Facet
#'
#' This function convert a from.json.Facet to a Facet
#' @param json.object Facet json object
#' @return Facet
#'
from.json.Facet <- function(json.object){
    localFacetValues <- c(MISSING_VALUE)
    if (!(is.null(json.object$facetValues) || (length(json.object$facetValues) == 0))){
        localFacetValues <- lapply(json.object$facetValues,function(x){
            from.json.FacetValue(x)
        })
    }
    res <- new("Facet",
               facet.id    = json.object$id,
               total       = json.object$total,
               facetValues = localFacetValues,
               label = json.object$label
    )
    return(res)
}

#' from.json.DatasetSummary
#'
#' This function converts a json object to a DatasetSummary
#'
#' @param json.object DatasetSummary json Object
#' @return DatasetSummary
#'
from.json.DatasetSummary <- function(json.object){

    localOrganisms <- c(MISSING_VALUE)
    if (!(is.null(json.object$organisms) || (length(json.object$organisms) == 0))){
        localOrganisms <- lapply(json.object$organisms,function(x){from.json.Organism(x)})
    }


    res <- new("DatasetSummary",
               dataset.id  = json.object$id,
               description = ifelse(is.null(json.object$description) || (length(json.object$description) == 0),
                                    MISSING_VALUE, json.object$description),
               database    = json.object$source,
               keywords    = ifelse(is.null(json.object$keywords) || (length(json.object$keywords) == 0),
                                    c(MISSING_VALUE),json.object$keywords),
               publication.date = ifelse(is.null(json.object$publicationDate) || (length(json.object$publicationDate) == 0), MISSING_VALUE, json.object$publicationDate),
               organisms = localOrganisms,
               title   = ifelse(is.null(json.object$title) || (length(json.object$title) == 0),
                                MISSING_VALUE, json.object$title),
               visit.count = json.object$viewsCount,
               score = ifelse(is.null(json.object$score), MISSING_VALUE, json.object$score),
               omics.type  = ifelse(is.null(json.object$omicsType), MISSING_VALUE, json.object$omicsType)
    )
    return(res)
}

#' from.json.DatasetResults
#'
#' This function get information from DatasetResults
#' @param json.object DatasetResult json object
#' @return DatasetResult
#'
from.json.DatasetResults <- function(json.object){
    localFacets <- c(MISSING_VALUE)
    if (!(is.null(json.object$facets) || (length(json.object$facets) == 0))) {
        localFacets <- lapply(json.object$facets,function(x){
            from.json.Facet(x)
        }
        )
    }
    localDatasets <- c(MISSING_VALUE)
    if (!(is.null(json.object$datasets) || (length(json.object$datasets) == 0))) {
        localDatasets <- lapply(json.object$datasets,function(x){
            from.json.DatasetSummary(x)
        }
        )
    }

    res  <- new("DatasetResult",
                count  = json.object$count,
                facets = localFacets,
                datasets = localDatasets
    )
    return(res)
}

setMethod("show",
          signature = "DatasetDetail",
          definition = function(object) {
              cat("An object of class ", class(object), "\n", sep="")
              cat("    Name: ", object$name, "\n", sep="")
              cat("    Dataset Id: ", object$dataset.id, "\n", sep="")
              cat("    Database: ", object$database, "\n", sep="")
              cat("    Description: ", object$description, "\n", sep="")
              invisible(NULL)
          }
)

#' from.json.BiologicalSimilars
#'
#' This function get information from DatasetResults
#' @param similars json object
#' @return Return the Biological Similars of a dataset
#' @export
#'
from.json.BiologicalSimilars <- function(json.object){
    res  <- new("Similar",
                dataset.id  = json.object$id,
                database    = json.object$source,
                score       = json.object$score
    )
    return(res)
}
#' from.json.BiologicalSimilarsList
#'
#' This function get information from DatasetResults
#' @param similars json object
#' @param accession accesioon of the current dataset
#' @param database Database
#' @return BiologicalSimilars List
#' @export
#'
from.json.BiologicalSimilarsList <- function(accession = accession, database = database, similars = json.datasetDetail){

    localSimilars <- c(MISSING_VALUE)
    if(!is.null(similars) || !(is.null(similars$datasets) || (length(similars$datasets) == 0))){
        localSimilars <- lapply(similars$datasets,function(x){
            from.json.BiologicalSimilars(x)
        }
        )
    }

    res  <- new("DatasetSimilars",
                dataset.id  = accession,
                database    = database,
                similars    = localSimilars
    )
    return(res)
}

#' Returns a DatasetDetail from OmicsDI
#'
#' @param accession id in the original repository
#' @param database the original database where this dataset has been store
#' @return DatasetDetail
#' @author Yasset Perez-Riverol
#' @details TODO
#' @export
get.DatasetDetail <- function(accession, database) {
    datasetDetail <- tryCatch({
        json.datasetDetail <- RJSONIO::fromJSON(paste0(ddi_url, "/dataset/get", "?accession=", accession, "&database=", database), simplify = FALSE);
        return (from.json.DatasetDetail(json.datasetDetail));
    }, error = function(err) {
        print(paste("MY_ERROR:  ",err))
        return(NULL)
    });
}

#' search.DatasetsSummary
#'
#' This function search for dataset summaries in the resource
#'
#' @param query the current query of the search
#' @param sort.field the field to sort the results
#' @param start where to start the page
#' @param size how many elements to read
#' @param faceCount the information of all the facets for the search
#' @return DatasetResults
#' @export

search.DatasetsSummary <- function(query = "", start = 0, size = 20, faceCount = 20){
    json.datasetSummary <- RJSONIO::fromJSON(paste0(ddi_url, "/dataset/search", "?query=", query, "&start=", start, "&size=", size, "&faceCount=", faceCount), simplify = FALSE)
    # print(json.datasetSummary)
    datasetResults <- from.json.DatasetResults(json.datasetSummary)
    return(datasetResults)
}


#' get.MetadataSimilars
#'
#' Dataset Similars by Metadata
#'
#' @param database the database name
#' @param accession the dataset accession
#' @return DatasetResults
#' @export
#'
get.MetadataSimilars <- function(database = "", accession = ""){
    json.dasetSimilars <- fromJSON(file = paste0(ddi_url, "/dataset/getSimilar", "?acc=", accession, "&database=", database))
    datasetResults <- from.json.DatasetResults(json.dasetSimilars)
    return(datasetResults)
}

#' get.BiologicalSimilars
#' Returns a Biological Molecules similarity for a Dataset
#'
#' @param accession id in the original repository
#' @param database the original database where this dataset has been store
#' @return List of similars
#' @author Yasset Perez-Riverol
#' @export
get.BiologicalSimilars <- function(accession, database) {
    datasetDetail <- tryCatch({
        json.datasetDetail <- RJSONIO::fromJSON(paste0(ddi_url, "/enrichment/getSimilarDatasetsByBiologicalData", "?accession=", accession, "&database=", database), simplify = FALSE);
        return (from.json.BiologicalSimilarsList(accession = accession, database = database, similars = json.datasetDetail));
    }, error = function(err) {
        print(paste("MY_ERROR:  ",err))
        return(NULL)
    });
}

#' get.MetadataSimilars
#' Returns a Biological Molecules similarity for a Dataset
#'
#' @param accession id in the original repository
#' @param database the original database where this dataset has been store
#' @return List of similars
#' @author Yasset Perez-Riverol
#' @export

get.MetadataSimilars <- function(accession, database) {
    datasetDetail <- tryCatch({
        json.datasetDetail <- RJSONIO::fromJSON(paste0(ddi_url, "/dataset/getSimilar", "?acc=", accession, "&database=", database), simplify = FALSE);
        return (from.json.BiologicalSimilarsList(accession = accession, database = database, similars = json.datasetDetail));
    }, error = function(err) {
        print(paste("MY_ERROR:  ",err))
        return(NULL)
    });
}
