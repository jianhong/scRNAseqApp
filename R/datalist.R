getDataSets <- function(datasets, appconf) {
    if(missing(datasets)){
        datasets <- list.dirs(
            .globals$datafolder,
            full.names = FALSE,
            recursive = FALSE)
        datasets <- datasets[vapply(
            datasets,
            FUN = checkFiles,
            FUN.VALUE = logical(1L))]
    }
    if (!missing(appconf)) {
        if(all(datasets==names(appconf))){
            n <- vapply(appconf, function(.ele)
                .ele$title,
                FUN.VALUE = character(1))
            if (length(n) == length(datasets)) {
                names(datasets) <- n
                datasets <- datasets[order(names(datasets))]
            }
        }
    }
    return(datasets)
}

getNamedDataSets <- function(datasets, appconf) {
    if(missing(datasets)){
        datasets <- getDataSets()
    }
    if(missing(appconf)){
        appconf <- getAppConf(datasets=datasets)
    }
    nds <- getDataSets(datasets = datasets, appconf = appconf)
}

# check if all the required files are available
checkFiles <- function(folder) {
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

getDefaultDataset <- function(defaultDataset = "pbmc_small", datasets) {
    if(missing(datasets)) datasets <- getDataSets()
    if (!defaultDataset %in% datasets) {
        defaultDataset <- datasets[1]
    }
    defaultDataset
}

getAppConf <- function(datasets) {
    if(missing(datasets)) datasets <- getDataSets()
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

updateSymbolDict <- function(datasets) {
    if(missing(datasets)) datasets <- getDataSets()
    symbols <- lapply(datasets, function(.ele) {
        names(readData("sc1gene", .ele))
    })
    sort(unique(unlist(symbols)))
}

getRef <- function(dataset, key, appconf) {
    stopifnot(!missing(appconf))
    stopifnot(!missing(dataset))
    stopifnot(!missing(key))
    if (key %in% names(appconf[[dataset]][["ref"]])) {
        appconf[[dataset]][["ref"]][[key]]
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

getToken <- function() {
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
