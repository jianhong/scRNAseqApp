#' The methods for \link{APPconf-class}
#' @description The assessment and replacement methods for \link{APPconf-class}
#' @name APPconf-methods
#' @family APPconf
#' @rdname APPconf-methods
#' @param object an object of APPconf
#' @exportMethod show
#' @aliases show,APPconf-method
#' appconf <- readRDS(system.file("extdata", "data",
#'  "pbmc_small", "appconf.rds", package="scRNAseqApp"))
#' appconf
setMethod("show", "APPconf", function(object){
  cat("This is an object of APPconf \n")
  cat("describe the data ", object@title, " saved in ", object@id, "\n")
  if(length(object@ref$bib)){
    cat("Please cite the data via ", object@ref$bib, "\n")
  }else{
    if(length(object@ref$entry)){
      cat("Please cite the data via ", object@ref$entry, "\n")
    }
  }
  cat("First 5 markers for current data: ", head(object@markers, n=5L), "\n")
  cat("Keywords for current data: ", head(object@keywords, n=20L), "\n")
})

#' @rdname APPconf-methods
#' @export
#' @param x APPconf object.
setMethod("$", "APPconf", function(x, name) slot(x, name))
#' @rdname APPconf-methods
#' @param name A literal character string or a name (possibly backtick quoted).
#' @param value value to replace.
#' @export
#' @examples
#' appconf$title
setReplaceMethod("$", "APPconf",
                 function(x, name, value){
                   slot(x, name, check = TRUE) <- value
                   x
                 })


#' @rdname APPconf-methods
#' @export
#' @param i,j indices specifying elements to extract or replace.
#' @param \dots Named or unnamed arguments to form a signature.
#' @param exact see \link[base]{Extract}
setMethod("[[", "APPconf", function(x, i, j, ..., exact=TRUE) slot(x, i))
#' @rdname APPconf-methods
#' @export
#' @examples
#' appconf[["title"]]
setReplaceMethod("[[", "APPconf",
                 function(x, i, ..., value){
                   slot(x, i, check = TRUE) <- value
                   x
                 })


