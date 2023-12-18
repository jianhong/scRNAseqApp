# rename the function to compatable with old functions
getDataSets <- function(datasets, appconf, privilege=NULL) {
    datasets <- listDatasets(key = datasets,
                             privilege=privilege,
                             named=missing(appconf))
    return(datasets)
}
# rename the old getDataSets function to checkAvailableDataSets
# to check the datasets by list the folders
checkAvailableDataSets <- function(privilege='all', token=NULL) {
    datasets <- list.dirs(
        .globals$datafolder,
        full.names = FALSE,
        recursive = FALSE)
    tokenList <- list()
    if(!is.null(token)){
        tokenList <- getTokenList()
    }
    datasets <- datasets[vapply(
        datasets,
        FUN = checkFiles,
        FUN.VALUE = logical(1L),
        privilege = privilege,
        token = token,
        tokenList = tokenList)]
    return(datasets)
}

getNamedDataSets <- function(privilege=NULL) {
    datasets <- checkAvailableDataSets(privilege = privilege)
    names(datasets) <- datasets
    appconf <- getAppConfObj(datasets=datasets, privilege=privilege)
    if(all(datasets==names(appconf))){
        n <- vapply(appconf, function(.ele)
            .ele$title,
            FUN.VALUE = character(1))
        if (length(n) == length(datasets)) {
            names(datasets) <- n
        }
    }
    datasets <- datasets[order(names(datasets))]
}

# check if all the required files are available
checkFiles <- function(folder, privilege, token, tokenList) {
    if(checkLocker(folder)){
        if(!checkPrivilege(privilege=privilege, datasetname = folder) &&
           !checkToken(tokenList=tokenList, token = token, dataset = folder)){
            return(FALSE)
        }
    }
    all(
        c(
            .globals$filenames$appconf,
            .globals$filenames$sc1conf,
            .globals$filenames$sc1def,
            .globals$filenames$sc1gene,
            .globals$filenames$sc1gexpr,
            .globals$filenames$sc1meta
        ) %in%
            list.files(
                file.path(.globals$datafolder, folder),
                full.names = FALSE)
    )
}

getDefaultDataset <- function(
        defaultDataset = "pbmc_small", datasets, privilege=NULL) {
    if(missing(datasets)) datasets <- getDataSets(privilege = privilege)
    if (!defaultDataset %in% datasets) {
        defaultDataset <- datasets[1]
    }
    defaultDataset
}

getAppConf <- function(datasets, privilege=NULL) {
    if(missing(datasets)) datasets <- getDataSets(privilege = privilege)
    appconf <- getConfigTable()
    appconf <- appconf[match(datasets, appconf$id), , drop=FALSE]
    keep <- !is.na(appconf$id)
    appconf <- appconf[keep, , drop=FALSE]
    appconf <- apply(appconf, 1, as.list)
    names(appconf) <- datasets[keep]
    return(appconf)
}

getAppConfObj <- function(datasets, privilege=NULL) {
    if(missing(datasets)){
        datasets <- checkAvailableDataSets(privilege = privilege)
    }
    appconf <- lapply(datasets, function(.ele) {
        conf <- readData("appconf", .ele)
        stopifnot(
            'id is not identical with the folder name' =
                .ele == conf$id)
        conf
    })
    names(appconf) <- datasets
    return(appconf)
}

getDataType <- function(appconf) {
    stopifnot(!missing(appconf))
    data_types <- vapply(appconf, function(.ele)
        .ele$type,
        FUN.VALUE = character(1))
}

updateDatasetForToken <- function(defaultDataset, datasets){
    datasets1 <- getDataSets(privilege = 'all')
    datasets <- datasets1[datasets1 %in% c(datasets, defaultDataset)]
    appconf <- getAppConf(datasets = datasets, privilege = 'all')
    datasets <- listDatasets(key = datasets,
                             privilege='all',
                             named=TRUE)
    data_types <- getDataType(appconf)
    return(list(datasets=datasets, appconf=appconf, data_types=data_types))
}

updateSymbolDict <- function(datasets, privilege=NULL, updateDB = FALSE) {
    if(missing(datasets)) datasets <- getDataSets(privilege = privilege)
    touchGeneTable(updateDB)
    symbols <- listGeneSymbols(datasets = datasets)
    symbols$symbol
}

getRef <- function(dataset, key, appconf) {
    stopifnot(!missing(appconf))
    stopifnot(!missing(dataset))
    stopifnot(!missing(key))
    if (key %in% names(appconf[[dataset]])) {
        appconf[[dataset]][[key]]
    } else{
        NA
    }
}

trimBib <- function(bib) {
    return(sub("^(<.*?>)\\[\\d+\\]", "\\1", gsub("^\\s+", "", bib)))
}
get_full_ref_list <- function(appconf, returnLen = FALSE) {
    ref <- lapply(appconf, function(.ele) {
        .ele <- .ele$ref
        if (!is.null(.ele$entry)) {
            bib <- format(.ele$entry, style = "html")
            bib <- trimBib(bib)
            return(list(TRUE, bib))
        }
        if (!is.null(.ele$bib)) {
            bib <- trimBib(.ele$bib)
            if (bib != "" && !is.na(bib)) {
                if (!is.null(.ele$doi)) {
                    bib <- paste(
                        bib,
                        paste0(
                            "<a href='https://doi.org/",
                            .ele$doi,
                            "'>",
                            .ele$doi,
                            "</a>"
                        )
                    )
                }
                if (!is.null(.ele$pmid)) {
                    bib <- paste(
                        bib,
                        paste0(
                            "<a href='https://www.ncbi.nlm.nih.gov/pubmed",
                            .ele$pmid,
                            "'>PMID:",
                            .ele$pmid,
                            "</a>"
                        )
                    )
                }
                return(list(TRUE, bib))
            }
        }
        return(FALSE)
    })
    keep <- vapply(ref, function(.ele)
        .ele[[1]], logical(1L))
    ref <- ref[keep]
    ref <- unlist(lapply(ref, function(.ele)
        .ele[[2]]))
    ref <- sort(ref)
    ref <- unique(ref)
    if (returnLen)
        return(length(ref))
    ref <- paste("<li>", ref, "</li>")
    ref <- paste("<ol>\n", paste(ref, collapse = "\n"), "\n</ol>")
    HTML(ref)
}

getTokenList <- function() {
    token <- dir(
        .globals$datafolder,
        .globals$filenames$token,
        recursive = TRUE,
        full.names = TRUE
    )
    token_n <- lapply(token, readLines)
    token <- rep(basename(dirname(token)), lengths(token_n))
    names(token) <- unlist(token_n)
    token <- as.list(token)
}

checkLocker <- function(datasetname) {
    file.exists(file.path(
        .globals$datafolder,
        datasetname,
        .globals$filenames$locker
    ))
}

checkToken <- function(tokenList, token, dataset) {
    if(length(token)==0){
        return(FALSE)
    }
    if (token %in% names(tokenList)) {
        return(tokenList[[token]] == dataset)
    } else{
        return(FALSE)
    }
}

checkPrivilege <- function(privilege, datasetname) {
    if (is.null(privilege))
        return(FALSE)
    privilege == "all" || grepl(datasetname, privilege, fixed = TRUE)
}

checkMisc <- function(slot, folder) {
    file.exists(file.path(
        .globals$datafolder,
        folder,
        .globals$filenames[[slot]]))
}
