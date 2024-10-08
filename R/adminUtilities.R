adminMsg <- function(
        msg,
        type,
        duration = 5,
        close = TRUE) {
    showNotification(
        toString(msg)[1],
        duration = duration,
        closeButton = close,
        type = type
    )
}

conditionHandler <- function(cond) {
    type <- 
        vapply(c("warning", "message", "error"), function(x)
            is(cond, x),
            logical(1L))
    type <- c("warning", "message", "error")[type]
    if (length(type) == 0)
        type <- 'warning'
    if (type == "message") {
        progress <- get("progress_value", envir = .globals)
        if (progress < .9) {
            progress <- progress + .05
        }
        assign("progress_value", progress, envir = .globals)
        get("progress",
            envir = .globals)$set(message = cond$message, value = progress)
    } else{
        adminMsg(
            cond$message,
            type = type[1],
            duration = 5,
            close = TRUE
        )
    }
}

#' @noRd
#' @param startMsg,endMsg messages for start and end of the progress
#' @param expr expression for the process
adminProcess <- function(expr, startMsg, endMsg, duration=5) {
    # Create a Progress object
    assign("progress", shiny::Progress$new(), envir = .globals)
    assign("progress_value", 0, envir = .globals)
    on.exit(get("progress", envir = .globals)$close())
    get("progress", envir = .globals)$set(
        message = startMsg, value = 0)
    withCallingHandlers(
        withRestarts({
            expr
        }, muffleStop = function()
            NULL),
        message = conditionHandler,
        warning = conditionHandler,
        error = function(e) {
            conditionHandler(e)
            invokeRestart("muffleStop")
        }
    )
    get("progress", envir = .globals)$close()
    on.exit()
    if (!missing(endMsg)) {
        adminMsg(endMsg, "message", duration = duration)
    }
}

askNamespace <- function(...) {
    pkgs <- list(...)
    lapply(pkgs, function(pkg) {
        if (!requireNamespace(pkg)) {
            adminMsg(paste(
                "The", pkg, "package is required for this function!"),
                type = "error")
            return()
        }
    })
}

redirectOutput <- function(
        input,
        output,
        session,
        open = TRUE,
        tmpf = tempfile()) {
    if (open) {
        sink(tmpf)
        autoInvalidate <- reactiveTimer(1000)
        observe({
            autoInvalidate()
            output$consoleOutput <-
                renderText(readLines(tmpf), sep = "\n")
        })
        
    } else{
        sink()
        autoInvalidate <- reactiveTimer(1000000)
    }
}

#' @noRd
#' @importFrom xml2 read_xml as_list
#' @param id id to be convert
#' @param type target id type
#' @param url the service url
idConverter <- function(
        id,
        type = c("doi", "pmid"),
        url = 'https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/') {
    type <- match.arg(type)
    res <- read_xml(paste0(url, "?format=xml&ids=", id))
    res <- as_list(res)
    if (!is.null(res$pmcids$record)) {
        return(attr(res$pmcids$record, type))
    }
    return(NULL)
}
updateRefIDs <- function(element, input, output, session) {
    o_ele <- ifelse(element == "doi", "pmid", "doi")
    updateID <- FALSE
    updateID <- is.null(input[[o_ele]])
    if (!updateID) {
        updateID <- gsub("\\s+", '', input[[o_ele]]) == ""
    }
    
    if (updateID) {
        res <- idConverter(input[[element]], type = o_ele)
        if (!is.null(res)) {
            updateTextInput(session,
                            o_ele,
                            value = res)
        } else{
            adminMsg(
                paste('Please update the correct', toupper(o_ele)),
                'warning')
        }
    }
}

#' @importFrom methods is
updateRefById <-
    function(id, element, FUN, input, output, session) {
        if (!is.null(input[[element]])) {
            if (input[[element]] != "") {
                tryCatch({
                    bibentry <- FUN(input[[element]])
                    if (is(bibentry, "bibentry")) {
                        updateTextAreaInput(
                            session,
                            id,
                            value = format(bibentry, style = 'html'))
                    }
                    updateRefIDs(element, input, output, session)
                    return(bibentry)
                },
                error = function(e) {
                    adminMsg(e, 'error')
                },
                warning = function(w) {
                    adminMsg(w, 'warning')
                })
                
            }
        }
    }
getGrpIDs <- function(config) {
    x <- config$ID[config$grp]
    names(x) <- x
    x
}
filterGrpIDs <- function(grp_ids, meta) {
    keep <- vapply(
        grp_ids,
        FUN = function(.ele) {
            all(is.na(char2numeric(as.character(
                unique(meta[[.ele]])
            ))))
        },
        FUN.VALUE = logical(1L)
    )
    grp_ids[keep]
}

#' @importFrom utils bibentry
updateAppConf <- function(input, global) {
    markers <- global()$markers
    if (is.character(markers)) {
        markers <- markers[!is.na(markers)]
        markers <- markers[markers != ""]
        markers <- t(t(markers))
        rownames(markers) <- markers
        markers <- list(markers = as.data.frame(markers))
    }
    if(is.list(markers) && !is.data.frame(markers)){
        if(!all(vapply(markers, is.data.frame, logical(1L)))){
            markers <- as.data.frame(markers)
        }
    }
    if(is.data.frame(markers)){
        markers <- list(markers)
    }
    if (input$species2 != "" && input$species2 != "NA") {
        species <- input$species2
    } else{
        species <- input$species
    }
    entry <- global()$ref
    if(!is(entry, 'bibentry')){
        author <- 'NA'
        journal <- 'unpublished'
        year <- format(Sys.time(), '%Y')
        entry <- bibentry(
            bibtype = 'Article',
            title = input$title,
            doi = input$doi,
            pmid = input$pmid,
            author = author,
            journal = journal,
            year = year,
            abstract = input$abstract
        )
    }
    appconf <- APPconf(
        title = input$title,
        id = input$dir,
        species = species,
        ref = list(
            bib = input$reference,
            doi = input$doi,
            pmid = input$pmid,
            entry = entry
        ),
        type = input$datatype,
        markers = markers,
        keywords = input$keywords
    )
    if (!is.null(input$dir)) {
        if (input$dir != "") {
            saveAppConf(appconf)
            updateConfigTable(appconf)
        }
    }
}
updateDef <- function(input) {
    sc1def <- list(
        meta1 = input$meta1,
        meta2 = input$meta2,
        gene1 = input$gene1,
        gene2 = input$gene2,
        genes = input$multigene,
        dimred = c(input$dimred1, input$dimred2),
        grp1 = input$grp1,
        grp2 = input$grp2
    )
    if (!is.null(input$dir)) {
        if (input$dir != "") {
            saveData(sc1def, input$dir, "sc1def")
        }
    }
}
formatfID_CL <- function(x, rev = FALSE) {
    if (rev) {
        x[!is.na(x)] <- gsub(";\\s+", "|", x[!is.na(x)])
        return(x)
    }
    x[!is.na(x)] <- gsub("\\|", "; ", x[!is.na(x)])
    x
}
#' read expression matrix from sc1gexpr.h5
#' @noRd
#' @param h5filename Parent foldername of h5 file
#' @param rown,coln rownames and colnames for the expression file.
#' rownames = names(readRDS('sc1gene.rds'));
#' colnames = readRDS('sc1meta.rds')$sampleID
#' @importFrom rhdf5 h5read
readDataMatrix <- function(h5filename, rown, coln) {
    expr <- h5read(
        file.path(
            .globals$datafolder,
            h5filename,
            .globals$filenames$sc1gexpr
        ),
        .globals$h5fGrp
    )
    rownames(expr) <- rown
    colnames(expr) <- coln
    return(expr)
}

getReductionMethod <-
    function(reduction_method, forMonocole = TRUE) {
        if ("umap" %in% reduction_method) {
            reduction_method <- "UMAP"
        } else{
            if ("tsne" %in% reduction_method) {
                reduction_method <- "tSNE"
            } else{
                if ("pca" %in% reduction_method) {
                    reduction_method <- "PCA"
                } else{
                    reduction_method <- "UMAP"
                }
            }
        }
        if (!forMonocole)
            reduction_method <- tolower(reduction_method)
        reduction_method
    }

# updateMetaData by double click the info plot
# if it is color, change color
# if it is label, change label
updateMetaData <- function(dataset, inpConf, inpMeta, privilege,
                           info, oldvalue, newvalue){
    check <- FALSE
    if(checkPrivilege(privilege, dataset)){
        if(!info %in% inpConf$UI){
            if(info=='sampleID'){
                adminMsg(
                    'sampleID is not a proper info name.',
                    type = 'warning',
                    duration = 5,
                    close = TRUE
                )
            }else if(newvalue=='duplicate'){
                newvalue <- inpConf[inpConf$UI==oldvalue]
                id <- newvalue$ID
                newvalue$UI <- info
                newvalue$ID <- make.names(info)
                inpConf <- rbind(inpConf, newvalue)
                inpMeta[[newvalue$ID]] <- inpMeta[[id]]
                saveData(inpMeta, dataset, "sc1meta")
                saveData(inpConf, dataset, "sc1conf")
                check <- TRUE
            }else if(newvalue=='rename'){
                newvalue <- inpConf[inpConf$UI==oldvalue]
                id <- newvalue$ID
                newvalue$UI <- info
                newvalue$ID <- make.names(info)
                inpConf[inpConf$UI==oldvalue] <- newvalue
                colnames(inpMeta)[colnames(inpMeta)==id] <- newvalue$ID
                saveData(inpMeta, dataset, "sc1meta")
                saveData(inpConf, dataset, "sc1conf")
                check <- TRUE
            }else if(newvalue=='delete'){
                newvalue <- inpConf[inpConf$UI==oldvalue]
                #inpMeta[, (newvalue$ID):=NULL]
                inpConf <- inpConf[inpConf$UI!=oldvalue]
                saveData(inpMeta, dataset, "sc1meta")
                saveData(inpConf, dataset, "sc1conf")
                check <- TRUE
            }
        }else{
            id <- inpConf[inpConf$UI==info]$ID
            fID <- strsplit(inpConf[inpConf$UI==info]$fID, '\\|')[[1]]
            fCL <- strsplit(inpConf[inpConf$UI==info]$fCL, '\\|')[[1]]
            if(oldvalue %in% fID){
                if(grepl('Merging to:', newvalue)){
                    newvalue <- sub('Merging to:', '', newvalue)
                    newvalue <- sub('^ *', '', newvalue)
                    nCL <- fCL[which(fID==newvalue)]
                    idx <- which(fID==oldvalue)
                    fCL <- fCL[-idx]
                    fID <- fID[-idx]
                    lfactor <- is.factor(inpMeta[[info]])
                    l <- inpMeta[[info]]
                    if(lfactor){
                        l <- as.character(l)
                    }
                    l[which(l==oldvalue)] <- newvalue
                    if(lfactor){
                        inpMeta[[info]] <- factor(l, levels = fID)
                    }
                    fID <- paste(fID, collapse = '|')
                    inpConf[inpConf$UI==info, 'fID'] <- fID
                    fCL <- paste(fCL, collapse = '|')
                    inpConf[inpConf$UI==info, 'fCL'] <- fCL
                    saveData(inpMeta, dataset, "sc1meta")
                    saveData(inpConf, dataset, "sc1conf")
                    check <- TRUE
                }else if(!newvalue %in% fID){
                    fID[which(fID==oldvalue)] <- newvalue
                    lfactor <- is.factor(inpMeta[[info]])
                    l <- inpMeta[[info]]
                    if(lfactor){
                        l <- as.character(l)
                    }
                    l[which(l==oldvalue)] <- newvalue
                    if(lfactor) inpMeta[[info]] <- factor(l, levels = fID)
                    fID <- paste(fID, collapse = '|')
                    inpConf[inpConf$UI==info, 'fID'] <- fID
                    saveData(inpMeta, dataset, "sc1meta")
                    saveData(inpConf, dataset, "sc1conf")
                    check <- TRUE
                }else{
                    adminMsg(
                        'duplicated value. If you want to merge the cell info, please add "Merging to:" to the label.',
                        type = 'warning',
                        duration = 5,
                        close = TRUE
                    )
                }
            }else{
                if(oldvalue %in% fCL){
                    if(!newvalue %in% fCL){
                        fCL[which(fCL==oldvalue)] <- newvalue
                        fCL <- paste(fCL, collapse = '|')
                        inpConf[inpConf$UI==info, 'fCL'] <- fCL
                        saveData(inpConf, dataset, "sc1conf")
                        check <- TRUE
                    }else{
                        adminMsg(
                            'duplicated value',
                            type = 'warning',
                            duration = 5,
                            close = TRUE
                        )
                    }
                }
            }
        }
    }
    return(check)
}