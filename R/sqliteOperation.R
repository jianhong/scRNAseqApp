# sqlite operation
#' @importFrom DBI dbConnect dbDisconnect dbWriteTable
#'  dbGetQuery dbSendQuery
#' @importFrom RSQLite SQLite
connectDB <- function(FUN, ...){
    con <- dbConnect(SQLite(),
                     dbname = file.path(.globals$app_path,
                                        .globals$credential_path))
    on.exit(dbDisconnect(con))
    FUN(conn = con, ...)
}
#' @importFrom shinymanager read_db_decrypt
isEncrypted <- function(){
    db <- connectDB(read_db_decrypt, name = .globals$credentialTableName)
    identical(names(db), c("value", "iv"))
}
getCredential <- function(){
    res <- connectDB(read_db_decrypt, 
                     .globals$credentialTableName,
                     .globals$passphrase)
}
getConfigTable <- function(){
    res <- connectDB(read_db_decrypt, .globals$configTableName)
}
replaceNULL <- function(x, by=NA){
    ifelse(is.null(x), by, x)
}
createConfigTable <- function(appconf){
    appData <- lapply(appconf, FUN = function(.ele){
        c(title=.ele$title,
          id=.ele$id,
          species=.ele$species,
          type=.ele$type,
          markers=paste(markers(.ele), collapse = .globals$configTableSep),
          keywords=paste(.ele$keywords, collapse = .globals$configTableSep),
          groupCol=paste(.ele$groupCol, collapse = .globals$configTableSep),
          ref_bib=trimBib(replaceNULL(.ele$ref$bib)),
          ref_doi=replaceNULL(.ele$ref$doi),
          ref_pmid=replaceNULL(.ele$ref$pmid),
          ref_title=replaceNULL(.ele$ref$entry$title),
          ref_author=paste(.ele$ref$entry$author,
                           collapse = .globals$configTableSep),
          ref_year=replaceNULL(.ele$ref$entry$year),
          ref_journal=replaceNULL(.ele$ref$entry$journal),
          ref_abstract=replaceNULL(.ele$ref$entry$abstract))
    })
    appData <- do.call(rbind, appData)
    appData <- as.data.frame(appData)
    appData$locker <- vapply(appData$id, FUN = checkLocker,
                             FUN.VALUE = logical(1L))
    connectDB(dbWriteTable, name = .globals$configTableName,
              value = appData, overwrite = TRUE)
}

updateConfigTable <- function(){
    appconf <- getAppConfObj(privilege = 'all')
    createConfigTable(appconf)
}

updateConfigTblKey <- function(key, feild, value){
    if(missing(key) || missing(feild)){
        stop('Not proper query statement.')
    }
    query <- paste('UPDATE ', 
                   .globals$configTableName,
                   ' SET ', feild, '="', value, '"',
                   ' WHERE id="', key, '"')
    connectDB(dbSendQuery, statement = query)
}
updateLocker <- function(key, value){
    updateConfigTblKey(key = key, feild = 'locker', value = as.numeric(value))
}

checkKeyFromConfig <- function(key, feild, unique=TRUE){
    if(missing(key) || length(key)==0){
        # list full references
        query <- paste('SELECT', feild, 'FROM', .globals$configTableName)
    }else{
        # list references by key
        if(length(key)==1){
            query <- paste0('SELECT ', feild, ' FROM ',
                            .globals$configTableName,
                            ' WHERE id="', key, '"')
        }else{
            query <- paste0('SELECT ', feild, ' FROM ',
                            .globals$configTableName,
                            ' WHERE id in ("',
                            paste(key, collapse = '","'), '")')
        }
    }
    res <- unlist(connectDB(dbGetQuery, statement = query))
    if(unique){
        unique(sort(res))
    }else{
        res
    }
}
# list one or full references
listReferences <- function(key){
    ref <- checkKeyFromConfig(key, feild='ref_bib')
    ref <- ref[ref!=""]
    ref
}
makeSortedUnique <- function(x){
    if(length(names(x))==length(x)){
        x <- x[order(names(x))]
    }else{
        x <- sort(x)
    }
    x <- x[!duplicated(x)]
    x
}
# list all datasets
listDatasets <- function(key, privilege='', named=FALSE){
    if(is.null(privilege)){
        privilege <- ''
    }
    ds <- checkKeyFromConfig(key, feild='id', unique=FALSE)
    ds <- unname(ds)
    if(named){
        dsn <- checkKeyFromConfig(key, feild = 'title', unique = FALSE)
        names(ds) <- dsn
    }
    if(privilege=='all'){
        return(makeSortedUnique(ds))
    }
    locker <- checkKeyFromConfig(key, feild = 'locker', unique=FALSE)
    if(any(locker)){
        keep <- mapply(function(.ds, .locker){
            !.locker || grepl(.ds, privilege, fixed = TRUE)
        }, ds, locker)
        return(makeSortedUnique(ds[keep]))
    }else{
        return(makeSortedUnique(ds))
    }
}
# list all species
listSpecies <- function(key){
    checkKeyFromConfig(key, feild='species')
}

