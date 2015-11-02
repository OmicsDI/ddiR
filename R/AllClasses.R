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
                 return("'dataset.id' must be a single valid string")

             # check project.accession
             if (!is.character(object@accession) || nchar(object@accession) == 0 || is.na(object@accession))
                 return("'database' must be a single valid string")
         }
)

#' ContactDetail represents a PRIDE Archive assay's contact
#'
#' @export
#' @exportClass ContactDetail
setClass(
  "ContactDetail",

  slots = c(
            title = "character",
            first.name = "character",
            last.name = "character",
            email = "character",
            affiliation = "character"
          ),

  prototype = list(
                title = MISSING_VALUE,
                first.name = MISSING_VALUE,
                last.name = MISSING_VALUE,
                email = MISSING_VALUE,
                affiliation = MISSING_VALUE
              ),

  validity = function(object) {
    # check title
    if (!is.character(object@title) || nchar(object@title) == 0 || is.na(object@title))
      return("'title' must be a single valid string")

    # check first.name
    if (!is.character(object@first.name) || nchar(object@first.name) == 0 || is.na(object@first.name))
      return("'first.name' must be a single valid string")

    # check last.name
    if (!is.character(object@last.name) || nchar(object@last.name) == 0 || is.na(object@last.name))
      return("'last.name' must be a single valid string")

    # check email
    if (!is.character(object@email) || nchar(object@email) == 0 || !grepl("@", object@email) || is.na(object@email))
      return("'email' must be a single valid email address")

    # check affiliation
    if (!is.character(object@affiliation) || nchar(object@affiliation) == 0 || is.na(object@affiliation))
      return("'affiliation' must be a single valid string")
  }
)
