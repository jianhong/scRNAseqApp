setClassUnion("character_OR_NULL", c("character", "NULL"))

#' Class \code{"APPconf"}
#' @description Ano object of class \code{"APPconf"} represents
#'  the metadata for a dataset.
#' @aliases APPconf
#' @rdname APPconf-class
#' @slot title character(1). Title of the data
#' @slot id character(1). Tolder name of the data
#' @slot species character(1). species
#' @slot ref Reference information in a list with element bib, doi, pmid
#' and entry. Entry must be an object of \link[utils]{bibentry}
#' @slot type character(1). Type of the data, scRNAseq or scATACseq.
#' @slot markers list. A list of data.frame represents cell
#'  markers.
#' @slot keywords character. A vector of characters represents the
#'  keywords of the study.
#' @slot groupCol character. The key group column name to separate
#'  the cells.
#' @importFrom methods setClass representation prototype setMethod
#'  new `slot<-` setClassUnion
#' @export
#' @examples
#' appconf <- readRDS(system.file("extdata", "data",
#'     "pbmc_small", "appconf.rds", package="scRNAseqApp"))
#' appconf

setClass(
    "APPconf",
    representation = representation(
        title = "character",
        id = "character",
        species = "character",
        ref = "list",
        type = "character",
        markers = "list",
        keywords = "character",
        groupCol = "character_OR_NULL"
    ),
    prototype = prototype(
        title = "scRNAseqApp",
        id = 'sample_data',
        ref = list(),
        type = "scRNAseqApp",
        markers = list(),
        keywords = "",
        groupCol = NULL
    ),
    validity = function(object) {
        if (length(object@title) != 1) {
            return("title must be character(1L)")
        }
        if (length(object@id) != 1) {
            return("id must be character(1L)")
        }
        if (make.names(object@id) != object@id) {
            return("id must be a safe name.")
        }
        if (!object@type %in% c("scRNAseq", "scATACseq", "scMultiome")) {
            return("type must be scRNAseq, scATACseq or scMultiome")
        }
        if (length(object@ref$entry)) {
            if (!is(object@ref$entry, "bibentry")) {
                return("ref$entry must be an object of bibentry.")
            }
        }
        if (length(object@markers)) {
            out <- lapply(object@markers, function(.ele) {
                if (!is.data.frame(.ele)) {
                    return("markers must be a list of data.frame")
                }
                if (length(rownames(.ele)) != nrow(.ele)) {
                    return(
                        "markers must be a list of data.frame with rownames
                        and the rownames show be the marker name"
                    )
                }
                return(NULL)
            })
            out <- out[lengths(out) > 0]
            if (length(out)) {
                return(out)
            }
        }
        return(TRUE)
    }
)

#' @rdname APPconf-class
#' @param \dots Each argument in \dots becomes an slot in the new
#' \code{"APPconf"}-class.
#' @return A APPconf object.
#' @export
APPconf <- function(...) {
    new("APPconf", ...)
}
