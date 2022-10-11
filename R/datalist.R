getDataSets <- function(datafolder="data", appconf=NULL){
  datasets <- list.dirs(datafolder, full.names = FALSE, recursive = FALSE)
  if(!is.null(appconf)){
    names(datasets) <- vapply(appconf, function(.ele) .ele$title,
                              FUN.VALUE = character(1))
  }
  return(datasets)
}
getDefaultDataset <- function(defaultDataset="pbmc_small",
                              datafolder="data"){
  datasets <- getDataSets(datafolder = datafolder)
  if(!defaultDataset %in% datasets){
    defaultDataset <- datasets[1]
  }
  defaultDataset
}

getAppConf <- function(datafolder="data"){
  datasets <- getDataSets(datafolder = datafolder)
  appconf <- lapply(datasets, function(.ele){
    readRDS(file.path(datafolder, .ele, "appconf.rds"))
  })
  names(appconf) <- datasets
  return(appconf)
}

getDataType <- function(appconf){
  stopifnot(!missing(appconf))
  data_types <- vapply(appconf, function(.ele) .ele$types,
                       FUN.VALUE = character(1))
}

getRef <- function(dataset, key, appconf){
  stopifnot(!missing(appconf))
  stopifnot(!missing(dataset))
  stopifnot(!missing(key))
  if(key %in% names(appconf[[dataset]][["ref"]])){
    appconf[[dataset]][["ref"]][[key]]
  }else{
    NA
  }
}

getToken <- function(datafolder="data"){
  token <- dir(datafolder, "token", recursive = TRUE, full.names = TRUE)
  token_n <- lapply(token, readLines)
  token <- rep(basename(dirname(token)), lengths(token_n))
  names(token) <- unlist(token_n)
  token <- as.list(token)
}

checkLockedDataset <- function(datasetname, datafolder="data",
                               lockfilename="LOCKER"){
  file.exists(file.path(datafolder, datasetname, lockfilename))
}
checkToken <- function(tokenList, token, dataset){
  if(token %in% names(tokenList)){
    return(tokenList[[token]]==dataset)
  }else{
    return(FALSE)
  }
}
checkPrivilege <- function(privilege, datasetname){
  if(is.null(privilege)) return(FALSE)
  privilege == "all" || grepl(datasetname, privilege, fixed = TRUE)
}
