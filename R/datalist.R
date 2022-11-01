datafolder <- "data"
defaultDataset <- "GSM5023610_glial_app"
datasets <- dir(datafolder)
token <- dir(datafolder, "token", recursive = TRUE, full.names = TRUE)
token_n <- lapply(token, readLines)
token <- rep(basename(dirname(token)), lengths(token_n))
names(token) <- unlist(token_n)
token <- as.list(token)
## the datalist file is described in data.R.
# source(file.path(datafolder, "datalist.R"))
# if(file.exists(file.path(datafolder, "token.R"))){
#   source(file.path(datafolder, "token.R"))
# }
#' @include token.R
if(!defaultDataset %in% datasets){
  defaultDataset <- datasets[1]
}

appconf <- lapply(datasets, function(.ele){
  readRDS(file.path(datafolder, .ele, "appconf.rds"))
})
names(appconf) <- datasets
names(datasets) <- vapply(appconf, function(.ele) .ele$title,
                          FUN.VALUE = character(1))

data_types <- vapply(appconf, function(.ele) .ele$types,
                     FUN.VALUE = character(1))

getRef <- function(dataset, key){
  appconf[[dataset]][["ref"]][[key]]
}

get_full_ref_list<- function(){
  ref <- lapply(appconf, function(.ele){
    .ele <- .ele$ref
    paste(.ele$authors, paste0("<i>", .ele$title, "</i>"),
          .ele$journals, .ele$years,
          ifelse(!is.null(.ele$pmids),
                 paste0("https://www.ncbi.nlm.nih.gov/pubmed/",
                        .ele$pmids),
                 ''), sep=",")
  })
  ord <- vapply(appconf, function(.ele) .ele$ref$authors,
                FUN.VALUE = character(1L))
  keep <- !is.na(ord)
  ref <- ref[keep]
  ord <- ord[keep]
  ref <- unlist(ref[order(ord)])
  ref <- unique(ref)
  ref <- paste0("[", seq_along(ref), "] ", ref, "<br/><br/>")
  HTML(ref)
}
