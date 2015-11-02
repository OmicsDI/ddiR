MISSING_VALUE <- "Not available"

#' DatasetSummary, This class contains the information of one particuar dataset in OmicsDI
#'
#' @importFrom rjson fromJSON
#' @import methods
#' @export
#' @exportClass DatasetSummary
setClass("DatasetSummary",
  slots = c(
      dataset.id  = "character",
      description = "character",
      database  = "character",
      keywords  = "vector",
      publicationDate = "character",
      organisms = "vector",
      title   = "character",
      visit.count = "numeric"),

  prototype = list(
    dataset.id = MISSING_VALUE,
    description = MISSING_VALUE,
    database = MISSING_VALUE,
    keywords = MISSING_VALUE,
    publicationDate = MISSING_VALUE,
    title = MISSING_VALUE,
    organisms = MISSING_VALUE,
    visit.count = 0
  ),

  validity = function(object) {
    # check assay.accession
    if (!is.character(object@dataset.id) || nchar(object@dataset.id) == 0 || is.na(object@dataset.id))
      return("'dataset.id' must be a single valid string")

    # check project.accession
    if (!is.character(object@database) || nchar(object@database) == 0 || is.na(object@database))
      return("'database' must be a single valid string")

    # check file.size
    if (!is.numeric(object@visit.count) || object@visit.count < 0 || is.na(object@visit.count))
      return("'visit.count' must be a none negative number")

    return(TRUE)
  }
)

#' Organism return an organism entity including its name and accession
#'
#' @importFrom rjson fromJSON
#' @import methods
#' @export
#' @exportClass Organism

setClass("Organism",

         slots = c(
             name = "character",
             accession = "character"
         ),

         prototype = list(
             name = MISSING_VALUE,
             accession = MISSING_VALUE
         ),

         validity = function(object){
             # check assay.accession
             if (!is.character(object@name) || nchar(object@name) == 0 || is.na(object@name))
                 return("'name' must be a single valid string")

             # check project.accession
             if (!is.character(object@accession) || nchar(object@accession) == 0 || is.na(object@accession))
                 return("'accession' must be a single valid string")
         }
)

#'FacetValue provides the information about an specific Facet in the API
#'
#' @importFrom rjson fromJSON
#' @import methods
#' @export
#' @exportClass FacetValue
#'
setClass("FacetValue",
         slots = c(value = "character",
                   count = "character",
                   label = "character",
          ),
         prototype = list( value = MISSING_VALUE,
                           count = MISSING_VALUE,
                           label = MISSING_VALUE
         ),
         validity = function(object){
             # check assay.accession
             if (!is.character(object@value) || nchar(object@value) == 0 || is.na(object@value))
                 return("'value' must be a single valid string")

             # check project.accession
             if (!is.character(object@count) || nchar(object@count) == 0 || is.na(object@count))
                 return("'value' must be a single valid string")

             # check project.accession
             if (!is.character(object@label) || nchar(object@label) == 0 || is.na(object@label))
                 return("'label' must be a single valid string")

         }
)

#'DataSetReult provides a list of datasets for an specific query
#'
#' @importFrom rjson fromJSON
#' @import methods
#' @export
#' @exportClass DataSetResult
#'
setClass("DataSetResult",
         slots = c(
             count  = "numeric",
             facets = "vector",
             datasets = "vector"),

         prototype = list(
             count  = 0,
             facets = list(),
             datasets = list()
         )
)

#'Facet provides the information about an specific Facet in the API
#'
#' @importFrom rjson fromJSON
#' @import methods
#' @export
#' @exportClass Facet
#'
setClass("Facet",
         slots = c(
             facet.id = "character",
             total    = "numeric",
             facetValues = "vector",
             label = "character"),
         prototype = list(
             facet.id = MISSING_VALUE,
             label    = MISSING_VALUE,
             total    = 0,
             facetValues = list()
         )
)

