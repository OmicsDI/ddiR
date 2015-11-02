ddi_url     <- "http://wwwdev.ebi.ac.uk/Tools/ddi/ws"
ddi_url_dev <- "http://wwwdev.ebi.ac.uk/Tools/ddi/ws"

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
              cat("    keywords # ", object@keywords, "\n", sep="")
              cat("    organisms #: ", object@organisms, "\n", sep="")
              cat("    Count Visit #: ", object@visit.count, "\n", sep="")
              invisible(NULL)
          }
)

#' Returns a DatasetResult instance from a JSON string representation
#'
#' @param json_str The JSON object
#' @param file the name of a file to read the json_str from; this can also be a URL. Only one of json_str or file must be supplied.
#' @param method use the C implementation, or the older slower (and one day to be depricated) R implementation
#' @param unexpected.escape changed handling of unexpected escaped characters. Handling value should be one of "error", "skip", or "keep"; on unexpected characters issue an error, skip the character, or keep the character
#' @return The DatasetResult instance
#' @author Yasset Perez-Riverol
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
from.json.DataSetResult <- function(json.object) {
    res <- new("DatasetResult",
               title = json.object$title,
               dataset.id = json.object$id,
               # database = json.object$source,
               description = json.object$description,
               publication.date = json.object$publicationDate,
               keywords = ifelse(is.null(json.object$keywords)||(length(json.object$keywords)==0), c(MISSING_VALUE), json.object$keywords),
               organisms = ifelse(is.null(json.object$organisms)||(length(json.object$organisms)==0), c(MISSING_VALUE), lapply(json.object$organisms,
                                                                                                                               function (x) {
                                                                                                                                   from.json.Organism(x)
                                                                                                                                   }
                                                                                                                               )
                                  ),
               visit.count = json.object$visitCount
    )
    return (res)
}

#' from.json.Organism
#'
#' This function converts an Organism json object to Organism
#'
#' @param json.object
#' @return Organism
#'
from.json.Organism <- function(json.object){
        res <- new ("Organism",
                    name = json.object$name,
                    accession = json.object$acc
                    )
        return(res)
    }

#' from.json.Protocol This function converts a json Protocol object to a Protocol object
#'
#' @param json.object
#' @return Protocol Object
#'
from.json.Protocol <- function(json.object){
    res <- new("Protocol",
               name  = json.object$name,
               description = json.object$description
    )
    return(res)
}

#' from.json.LabMember
#'
#' This function convert objects from json to LabMember
#' @param json.object object
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
    res <- new("DatasetDetail",
               name         = json.object$name,
               dataset.id   = json.object$id,
               description  = json.object$description,
               database     = json.object$source,
               full.dataset.link  = json.object$full_dataset_link,
               publication.date   = json.object$publicationDate,
               protocols    = ifelse(is.null(json.object$protocols) || (length(json.object$protocols) == 0), c(MISSING_VALUE), lapply(json.object$protocols,function(x){
                   from.json.Protocol(x)
                   }
               )),
               keywords     = ifelse(is.null(json.object$keywords) || (length(json.object$keywords) == 0), c(MISSING_VALUE),json.object$keywords),
               tissues      = ifelse(is.null(json.object$tissues) || (length(json.object$tissues) == 0), c(MISSING_VALUE),json.object$tissues),
               diseases     = ifelse(is.null(json.object$diseases) || (length(json.object$diseases) == 0), c(MISSING_VALUE),json.object$diseases),
               instruments  = ifelse(is.null(json.object$instruments) || (length(json.object$instruments) == 0), c(MISSING_VALUE),json.object$instruments),
               experiment.type = ifelse(is.null(json.object$experimentType) || (length(json.object$experimentType) == 0), c(MISSING_VALUE),json.object$experimentType),
               publication.ids  = ifelse(is.null(json.object$publicationIds) || (length(json.object$publicationIds) == 0), c(MISSING_VALUE),json.object$publicationIds),
               organisms    = ifelse(is.null(json.object$organisms) || (length(json.object$organisms) == 0), c(MISSING_VALUE), lapply(json.object$organisms,function(x){
                   from.json.Organism(x)
               }
               )),
               lab.members    = ifelse(is.null(json.object$labMembers) || (length(json.object$labMembers) == 0), c(MISSING_VALUE), lapply(json.object$labMembers,function(x){
                   from.json.LabMember(x)
               }
               ))
    )
    return(res)
}


#' from.json.FacetValue
#'
#' This function converts a json FacetValue object to FaceTvalue
#' @param json.object
#' @return FacetValue
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
#' @param json.object
#' @return Facet
#'
from.json.Facet <- function(json.object){
    res <- new("Facet",
               facet.id    = json.object$id,
               total       = json.object$total,
               facetValues = ifelse(is.null(json.object$facetValues) || (length(json.object$facetValues) == 0), c(MISSING_VALUE),
                                    lapply(json.object$facetValues,function(x){
                                        from.json.FacetValue(x)
                                    })
               ),
               label = json.object$label
    )
    return(res)
}

#' from.json.DatasetSummary
#'
#' This function converts a json object to a DatasetSummary
#'
#' @param json.object
#' @return DatasetSummary
#'
from.json.DatasetSummary <- function(json.object){
    res <- new("DatasetSummary",
                dataset.id  = json.object$id,
                description = ifelse(is.null(json.object$description) || (length(json.object$description) == 0),
                                     MISSING_VALUE, json.object$description),
                database    = json.object$source,
                keywords    = ifelse(is.null(json.object$keywords) || (length(json.object$keywords) == 0),
                                     c(MISSING_VALUE),json.object$keywords),
                publication.date = ifelse(is.null(json.object$publicationDate) || (length(json.object$publicationDate) == 0), MISSING_VALUE, json.object$publicationDate),
                organisms = ifelse(is.null(json.object$organisms) || (length(json.object$organisms) == 0), c(MISSING_VALUE),
                                   lapply(json.object$organisms,function(x){
                                       from.json.Organism(x)
                                   }
                                   )
                ),
                title   = ifelse(is.null(json.object$title) || (length(json.object$title) == 0),
                                 MISSING_VALUE, json.object$title),
                visit.count = json.object$visitCount
               )
    return(res)
}

#' from.json.DatasetResults
#'
#' This function get information from DatasetResults
#' @param json.object
#' @return DatasetResult
#'
from.json.DatasetResults <- function(json.object){
    res <- new("DatasetResult",
               count  = json.object$count,
               facets = ifelse(is.null(json.object$facets) || (length(json.object$facets) == 0), c(MISSING_VALUE),
                                lapply(json.object$facets,function(x){
                                                                from.json.Facet(x)
                                                           }
                                      )
                              ),
               datasets = ifelse(is.null(json.object$datasets) || (length(json.object$datasets) == 0), c(MISSING_VALUE),
                                 lapply(json.object$datasets,function(x){
                                     from.json.DatasetSummary(x)
               }
               ))
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

#' Returns a DatasetDetail from OmicsDI
#'
#' @param accession id in the original repository
#' @param database the original database where this dataset has been store
#' @return DatasetDetail
#' @author Yasset Perez-Riverol
#' @details TODO
#' @importFrom rjson fromJSON
#' @export
get.DatasetDetail <- function(accession, database) {
    json.datsetDetail <- fromJSON(file = paste0(ddi_url, "/dataset/get", "?acc=", accession, "&database=", database), method = "C")
    datasetDetail <- from.json.DatasetDetail(json.datsetDetail)
    return(datasetDetail)
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
#' @importFrom rjson fromJSON
#' @export

search.DatasetsSummary <- function(query = "", start = 0, size = 20, faceCount = 20){
    json.datasetSummary <- fromJSON(file = paste0(ddi_url, "/dataset/search", "?query=", query, "&start=", start, "&size=", size, "&faceCount=", faceCount))
    datasetResults <- from.json.DatasetResults(json.datasetSummary)
    return(datasetResults)
}

