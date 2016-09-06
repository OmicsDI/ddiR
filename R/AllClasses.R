MISSING_VALUE <- "Not available"

#' DatasetSimilars, This class contains the information of one particuar dataset in OmicsDI
#'
#' @export
#' @exportClass DatasetSimilars
#'
setClass("DatasetSimilars",
         slots = c(
             dataset.id  = "character",
             database  = "character",
             similars  = "vector"
             ),

         prototype = list(
             dataset.id = MISSING_VALUE,
             database = MISSING_VALUE,
             similars   = list(MISSING_VALUE)
         ),

         validity = function(object) {
             # check assay.accession
             if (!is.character(object@dataset.id) || nchar(object@dataset.id) == 0 || is.na(object@dataset.id))
                 return("'dataset.id' must be a single valid string")

             # check project.accession
             if (!is.character(object@database) || nchar(object@database) == 0 || is.na(object@database))
                 return("'database' must be a single valid string")

             return(TRUE)
         }
)

#' Similar, This class contains the information of one particuar dataset in OmicsDI
#'
#' @export
#' @exportClass Similar
#'
setClass("Similar",
         slots = c(
             dataset.id  = "character",
             database  = "character",
             score  = "character"
         ),

         prototype = list(
             dataset.id = MISSING_VALUE,
             database = MISSING_VALUE,
             similars   = MISSING_VALUE
         ),

         validity = function(object) {
             # check assay.accession
             if (!is.character(object@dataset.id) || nchar(object@dataset.id) == 0 || is.na(object@dataset.id))
                 return("'dataset.id' must be a single valid string")

             # check project.accession
             if (!is.character(object@database) || nchar(object@database) == 0 || is.na(object@database))
                 return("'database' must be a single valid string")

             return(TRUE)
         }
)

#' DatasetSummary, This class contains the information of one particuar dataset in OmicsDI
#'
#' @export
#' @exportClass DatasetSummary
#'
setClass("DatasetSummary",
  slots = c(
      dataset.id  = "character",
      description = "character",
      database  = "character",
      keywords  = "vector",
      publication.date = "character",
      organisms = "vector",
      title   = "character",
      visit.count = "numeric",
<<<<<<< HEAD
      score = "character",
      omics.type = "character"),
=======
      omicsType   = "vector"),
>>>>>>> yasset

  prototype = list(
    dataset.id = MISSING_VALUE,
    description = MISSING_VALUE,
    database = MISSING_VALUE,
    keywords = MISSING_VALUE,
    publication.date = MISSING_VALUE,
    title = MISSING_VALUE,
    organisms = MISSING_VALUE,
    visit.count = 0,
<<<<<<< HEAD
    score = MISSING_VALUE,
    omics.type = MISSING_VALUE
=======
    omicsType   = MISSING_VALUE
>>>>>>> yasset
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
             # check name
             if (!is.character(object@name) || nchar(object@name) == 0 || is.na(object@name))
                 return("'name' must be a single valid string")
         }
)

#'FacetValue provides the information about an specific Facet in the API
#'
#' @export
#' @exportClass FacetValue
#'
setClass("FacetValue",
         slots = c(value = "character",
                   count = "character",
                   label = "character"
          ),
         prototype = list( value = MISSING_VALUE,
                           count = MISSING_VALUE,
                           label = MISSING_VALUE
         ),
         validity = function(object){
             # check value
             if (!is.character(object@value) || nchar(object@value) == 0 || is.na(object@value))
                 return("'value' must be a single valid string")

             # check count
             if (!is.character(object@count) || nchar(object@count) == 0 || is.na(object@count))
                 return("'value' must be a single valid string")

             # check label
             if (!is.character(object@label) || nchar(object@label) == 0 || is.na(object@label))
                 return("'label' must be a single valid string")

         }
)

#' DatasetResult, provides a list of datasets for an specific query
#'
#' @export
#' @exportClass DatasetResult
#'
setClass("DatasetResult",
         representation(
             count  = "numeric",
             facets = "vector",
             datasets = "vector"
         ),

         prototype(
             count  = 0,
             facets = c(MISSING_VALUE),
             datasets = c(MISSING_VALUE)
         )
)

#'Facet provides the information about an specific Facet in the API
#'
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

#' DatasetDetail provides the information about an specific Facet in the API
#'
#' @export
#' @exportClass DatasetDetail
#'
setClass("DatasetDetail",
         slots = c(
             name         = "character",
             dataset.id   = "character",
             description  = "character",
             database     = "character",
             protocols     = "vector",
             keywords     = "list",
             tissues      = "list",
             diseases     = "list",
             full.dataset.link  = "character",
             instruments  = "list",
             experiment.type = "list",
             publication.date = "character",
             publication.ids  = "list",
             organisms        = "vector",
             lab.members      = "vector",
             omicsType        = "list",
             similars         = "vector"
             ),
         prototype = list(
             name = MISSING_VALUE,
             dataset.id  = MISSING_VALUE,
             description = MISSING_VALUE,
             database = MISSING_VALUE,
             protocols = list(MISSING_VALUE),
             keywords  = list(MISSING_VALUE),
             tissues      = list(MISSING_VALUE),
             diseases     = list(MISSING_VALUE),
             full.dataset.link  = MISSING_VALUE,
             instruments  = list(MISSING_VALUE),
             experiment.type = list(MISSING_VALUE),
             publication.date = MISSING_VALUE,
             publication.ids  = list(MISSING_VALUE),
             organisms        = list(MISSING_VALUE),
             lab.members      = list(MISSING_VALUE),
             omicsType        = list(MISSING_VALUE),
             similars         = list(MISSING_VALUE)
             ),
         validity = function(object){
             # name
             if (!is.character(object@name) || nchar(object@name) == 0 || is.na(object@name))
                 return("'name' must be a single valid string")

             # check project.accession
             if (!is.character(object@dataset.id) || nchar(object@dataset.id) == 0 || is.na(object@dataset.id))
                 return("'dataset.id' must be a single valid string")

             # check project.accession
             if (!is.character(object@database) || nchar(object@database) == 0 || is.na(object@database))
                 return("'database' must be a single valid string")
         }
)

#' Protocol retirve the information of a Protocol used in the experiment
#'
#' @export
#' @exportClass Protocol
#'
setClass("Protocol",

         slots = c(
             name = "character",
             description = "character"
        ),

        prototype = list(
            name = MISSING_VALUE,
            description = MISSING_VALUE
        ),

        validity = function(object){

            # check project.accession
            if (!is.character(object@name) || nchar(object@name) == 0 || is.na(object@name))
                return("'name' must be a single valid string")

            # check project.accession
            if (!is.character(object@description) || nchar(object@description) == 0 || is.na(object@description))
                return("'description' must be a single valid string")
        }
)
#' LabMember retrieve the information of a lab member including affilation, name, etc
#'
#' @export
#' @exportClass LabMember

setClass("LabMember",
         slots = c(
             name = "character",
             role = "character",
             affilation = "character",
             email = "character"
             ),

         prototype = list(
             name = MISSING_VALUE,
             role = MISSING_VALUE,
             affiliation = MISSING_VALUE,
             email = MISSING_VALUE)
)

#' StatRecord, This class contains the information of statistics records
#'
#' @export
#' @exportClass StatRecord

setClass("StatRecord",
         slots = c(
             name  = "character",
             value = "character",
             id    = "character",
             label = "character"),
         prototype = list(
             name  = MISSING_VALUE,
             value = MISSING_VALUE,
             id    = MISSING_VALUE,
             label = MISSING_VALUE),
         validity = function(object){

             # check name
             if (!is.character(object@name) || nchar(object@name) == 0 || is.na(object@name))
                 return("'name' must be a single valid string")

             # check value
             if (!is.character(object@value) || nchar(object@value) == 0 || is.na(object@value))
                 return("'value' must be a single valid string")

             # check id
             if (!is.character(object@id) || nchar(object@id) == 0 || is.na(object@id))
                 return("'id' must be a single valid string")

             # check label
             if (!is.character(object@label) || nchar(object@label) == 0 || is.na(object@label))
                 return("'label' must be a single valid string")
         }
)

#' DictWord, This class contains the information of a list of Dictionary Words
#'
#' @export
#' @exportClass DictWord
#'
setClass("DictWord",
         slots = c(
             total.count = "numeric",
             items = "vector"),
         prototype = list(
             total.count = 0,
             items = list()
        )
)

#' Item, This class contains the information of an Item in the Dictonary
#'
#' @export
#' @exportClass Item
#'
setClass("Item",
         slots = c(
             name = "character"
         ),

         prototype = list(
             name = MISSING_VALUE
         ),

         validity = function(object){
             # check name
             if (!is.character(object@name) || nchar(object@name) == 0 || is.na(object@name))
                 return("'name' must be a single valid string")
         }
)

#' Term, This class contains the information of a Term
#'
#' @export
#' @exportClass Term

setClass("Term",
         slots = c(
              label = "character",
              frequent = "character"
              ),

         prototype = list(
             label = MISSING_VALUE,
             frequent = MISSING_VALUE
         )
)

#' PiublicationResult, This class contains a list of publication for specific Query
#'
#' @export
#' @exportClass PublicationResult

setClass("PublicationResult",

         slots = c(count = "numeric",
                   publications = "vector"),

         prototype = list(
             count = 0,
             publications = list()
         )
)

#' PublicationDetail, This class contains a Publication Record
#'
#' @export
#' @exportClass PublicationDetail
#'
setClass("PublicationDetail",
         slots = c(
             id         = "character",
             date       = "character",
             database   = "character",
             keywords   = "vector",
             affilation = "vector",
             title      = "character",
             authors    = "vector",
             abstract   = "vector",
             journal    = "character",
             issue      = "character",
             pagination = "character",
             volume     = "character"
         ),

         prototype = list(
                 id         = MISSING_VALUE,
                 date       = MISSING_VALUE,
                 database   = MISSING_VALUE,
                 keywords   = list(),
                 affilation = list(),
                 title      = MISSING_VALUE,
                 authors    = list(),
                 abstract   = list(),
                 journal    = MISSING_VALUE,
                 issue      = MISSING_VALUE,
                 pagination = MISSING_VALUE,
                 volume     = MISSING_VALUE
             ),

         validity = function(object){

             # check id
             if (!is.character(object@id) || nchar(object@id) == 0 || is.na(object@id))
                 return("'id' must be a single valid string")

             # check date
             if (!is.character(object@date) || nchar(object@date) == 0 || is.na(object@date))
                 return("'date' must be a single valid string")

             # check database
             if (!is.character(object@database) || nchar(object@database) == 0 || is.na(object@database))
                 return("'database' must be a single valid string")

             # check title
             if (!is.character(object@title) || nchar(object@title) == 0 || is.na(object@title))
                 return("'title' must be a single valid string")

             # check journal
             if (!is.character(object@journal) || nchar(object@journal) == 0 || is.na(object@journal))
                 return("'journal' must be a single valid string")
         }
)
