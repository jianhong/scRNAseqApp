datafolder <- "data"
defaultDataset <- "GSM5023610_glial_app"
datasets <- dir(datafolder)
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
