#' The methods for \link{APPconf-class}
#' @description The assessment and replacement methods for \link{APPconf-class}
#' @name APPconf-methods
#' @family APPconf
#' @rdname APPconf-methods
#' @param object an object of APPconf
#' @importFrom methods show
#' @export
#' @aliases show,APPconf-method
#' @examples
#' appconf <- readRDS(system.file("extdata", "data",
#'     "pbmc_small", "appconf.rds", package="scRNAseqApp"))
#' appconf
setMethod("show", "APPconf", function(object) {
    cat("This is an object of APPconf \n")
    cat(
        "describe the data ",
        object@title,
        " saved in ",
        object@id,
        "\n")
    if (length(object@ref$bib)) {
        cat("Please cite the data via ", object@ref$bib, "\n")
    } else{
        if (length(object@ref$entry)) {
            cat("Please cite the data via ", object@ref$entry, "\n")
        }
    }
    cat(
        "First 5 markers for current data: ",
        head(markers(object), n = 5L),
        "\n")
    cat(
        "Keywords for current data: ",
        head(object@keywords, n = 20L),
        "\n")
})

#' @rdname APPconf-methods
#' @export
#' @param x APPconf object.
setMethod("$", "APPconf", function(x, name)
    slot(x, name))
#' @rdname APPconf-methods
#' @param name A literal character string or a name (possibly backtick quoted).
#' @param value value to replace.
#' @export
#' @examples
#' appconf$title
setReplaceMethod(
    "$",
    "APPconf",
    function(x, name, value) {
        slot(x, name, check = TRUE) <- value
        x
    })


#' @rdname APPconf-methods
#' @export
#' @param i,j indices specifying elements to extract or replace.
#' @param \dots Named or unnamed arguments to form a signature.
#' @param exact see \link[base]{Extract}
setMethod("[[", "APPconf", function(x, i, j, ..., exact = TRUE)
    slot(x, i))
#' @rdname APPconf-methods
#' @export
#' @examples
#' appconf[["title"]]
setReplaceMethod(
    "[[",
    "APPconf",
    function(x, i, ..., value) {
        slot(x, i, check = TRUE) <- value
        x
    })
#' @rdname APPconf-methods
#' @export
#' @param drop see \link[base]{drop}
#' @importFrom methods as
setMethod(
    "[",
    "APPconf",
    function(x, i, j, ..., drop = TRUE) {
        as(x, "list")[i]
    })
if (length(getGeneric("as.list")) == 0) {
    setGeneric("as.list", function(x, ...)
        standardGeneric("as.list"))
}
#' @rdname APPconf-methods
#' @export
#' @importFrom methods slotNames setAs
#' @examples
#' as.list(appconf)
setMethod(
    "as.list",
    "APPconf",
    function(x, ...) {
        ii <- slotNames(x)
        names(ii) <- ii
        lapply(ii, function(i)
            x[[i]])
    })
setAs(
    from = "APPconf",
    to = "list",
    function(from) {
        ii <- slotNames(from)
        names(ii) <- ii
        lapply(ii, function(i)
            from[[i]])
    })
if (length(getGeneric("as.character")) == 0) {
    setGeneric("as.character", function(x, ...)
        standardGeneric("as.character"))
}
#' @rdname APPconf-methods
#' @export
#' @importFrom methods slotNames setAs
#' @examples
#' as.character(appconf)
setMethod(
    "as.character",
    "APPconf",
    function(x, ...) {
        ii <- slotNames(x)
        c(markers(x), unlist(x[ii != 'markers']))
    })
setAs(
    from = "APPconf",
    to = "character",
    function(from) {
        ii <- slotNames(from)
        c(markers(from), unlist(from[ii != 'markers']))
    })

setGeneric("markers", function(x)
    standardGeneric("markers"))
#' @rdname APPconf-methods
#' @aliases markers
#' @export
#' @examples
#' markers(appconf)
setMethod("markers", "APPconf", function(x) {
    unique(unlist(lapply(x[["markers"]], rownames)))
})

if (length(getGeneric("lapply")) == 0) {
    setGeneric("lapply", function(X, FUN, ...)
        standardGeneric("lapply"))
}
#' @rdname APPconf-methods
#' @export
#' @param X an APPconf object.
#' @param FUN function used by `lapply`
#' @importFrom methods slotNames
#' @examples
#' lapply(appconf, print)
setMethod(
    "lapply",
    "APPconf",
    function(X, FUN, ...) {
        FUN <- match.fun(FUN)
        ii <- slotNames(X)
        names(ii) <- ii
        lapply(ii, function(i)
            FUN(X[[i]], ...))
    })
#' @rdname APPconf-methods
#' @export
#' @param recursive,use.names function used by \link[base:unlist]{unlist}
#' @return A named character vector.
#' @examples
#' unlist(appconf)
setMethod(
    "unlist",
    "APPconf",
    function(
        x,
        recursive = TRUE,
        use.names = TRUE) {
        unlist(as.list(x), recursive = recursive, use.names = use.names)
    })
