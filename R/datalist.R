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

