#' @importFrom htmltools htmlDependency
visitorDependencies <- function(){
    htmlDependency(
        name = "scRNAseqApp-assets", version = "0.0.1",
        package = "scRNAseqApp",
        src = "assets",
        script = c("js/script.js"),
        stylesheet = c("css/style.css")
    )
}
# parse querystring
parseQuery <- function(query, defaultDataset){
    stopifnot(is.list(query))
    from <- 'default'
    if (!is.null(query[['token']])) {
        token <- getTokenList()
        if (query[["token"]] %in% names(token)) {
            defaultDataset <- token[[query[['token']]]]
            from <- 'token'
        }
    } else {
        if (!is.null(query[['data']])) {
            defaultDataset <- query[['data']]
            from <- 'data'
        }
    }
    return(c(defaultDataset=defaultDataset, from=from))
}
# summary box for home page
summaryBox <- function(
        title, value,
        width = 4,
        icon = "fas fa-chart-bar",
        style = "info",
        border = "left") {
    div(
        class = c(paste0("col-md-", width), "about-left-border"),
        div(
            class = paste0(
                "card border-",
                border, "-",
                style,
                " shadow h-100 py-2"),
            div(
                class = "card-body",
                div(
                    class = "row no-gutters align-items-center",
                    div(
                        class = "col mr-2",
                        div(
                            class = paste0(
                                "text-xs font-weight-bold text-",
                                style, " text-uppercase mb-1"),
                            toupper(title)
                        ),
                        div(
                            class = "h5 mb-0 font-weight-bold text-gray-800",
                            value
                        )
                    ),
                    div(
                        class = "col-auto about-large-icon about-right",
                        icon(class=paste0("about-", style), icon)
                    )
                )
            )
        )
    )
}

plotLoader <- function(ui, bufferStr='loading...'){
    id <- removeQuote(gsub("^.*?id=(.*?)\\s+.*$", "\\1", ui))
    tagList(
        div(
            class = 'ploader-container',
            div(
                class = 'ploader',
                id=paste0("scRNAseqAppLoader-", id),
                bufferStr
            ),
            ui
        )
    )
}

#' Function to extract legend
#' @noRd
#' @importFrom ggplot2 ggplot_gtable ggplot_build
g_legend <- function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(vapply(tmp$grobs,
                        function(x) x$name, character(1L)) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}

# update search results
updateSearch <- function(
        key_words, datasets,
        auth, global, page=1,
        id, input, output, session){
    if( isGene(
        key_words,
        datasets,
        maxEvent = .globals$maxNumGene) ||
        mappingToSymbols(key_words)){## check if it is a gene
        search_res <- checkGene(
            key_words, datasets=datasets,
            auth = auth,
            global = global, page=page,
            id=id, input=input, output=output, session=session)
        output$search_res <-
            renderUI(search_res$UI)
        for(i in seq_along(search_res$PLOT)){
            local({
                output[[names(search_res$PLOT)[i]]] <- search_res$PLOT[[i]]
            })
        }
    }else{
        key_words <- strsplit(key_words, "\\s+")[[1]]
        key_words <- gsub("[^a-zA-Z0-9._'\"*-]+", "", key_words)
        res_data <- lapply(getAppConf(privilege = auth$privilege), function(.ele){
            x <- paste(as.character(.ele), collapse = " ")
            m <- vapply(key_words, grepl, logical(1L), x = x, ignore.case=TRUE)
            m <- sum(m)
            return(c(m, .ele$id, .ele$title))
        })
        ## update search_res
        res_data <- do.call(rbind, res_data)
        res_data <- res_data[res_data[, 1]>0, , drop=FALSE]
        res_data <- res_data[
            order(res_data[, 1], decreasing = TRUE), -1, drop=FALSE]
        if(nrow(res_data)>0){
            output$search_res <- renderUI(
                tags$ul(
                    class='about-ul',
                    apply(res_data, 1, function(.ele){
                        return(tags$li(
                            tags$a(
                                href=paste0('?data=', .ele[1]),
                                .ele[2])))
                    })
                )
            )
        }else{
            output$search_res <- renderUI(tags$div(
                "Sorry, I can do nothing with this. Try different one.
                If you see this sentence is fading out,
                I'm working hard on your request.
                Have a coffee."))
        }
    }
}

#' convert search keys to gene symbols
#' @noRd
#' @param x the key words to be converted
#' @param transform convert the x to gene symbol
mappingToSymbols <- function(x, transform=FALSE){
    # quote all gene symbols
    # convert names in database to gene symbols
    rs <- unique(mapGeneSymbols(x)$gene)
    if(!transform){
        return(length(rs)>0)
    }else{
        if(length(rs)>0){
            if(length(rs)==1){
                return(paste0('"', rs, '"'))
            }else{
                return(rs)
            }
        }else{
            return(x)
        }
    }
}

#' check if a symbol is a gene
#' @noRd
#' @param symbol the character to be checked
#' @param datasets the available datasets
isGene <- function(symbol, datasets, maxEvent=3){
    if(isQuote(symbol)){
        symbol <- removeQuote(symbol)
        g <- listGeneSymbols(genes = symbol, datasets = datasets)
        return(length(g)>0)
    }
    if(isAsterisk(symbol)){
        symbol <- isAsterisk(symbol, transform = TRUE, to = '%')
        maxEvent <- .globals$maxNumGene
        like <- TRUE
    }else{
        like <- FALSE
    }
    g <- listGeneSymbols(genes = symbol, datasets = datasets, like=like)
    length(g) > 0 && length(g) < maxEvent
}
#' check if a symbol is quoted
#' @noRd
#' @param symbol the character to be checked
isQuote <- function(symbol){
    grepl("'|\"", symbol)
}
removeQuote <- function(symbol){
    gsub("'|\"", "", symbol)
}
#' check if a symbol contain '*'
#' @noRd
#' @param symbol the character to be checked
#' @param transform change the '*' to '.*'
#' @param to The character to be changed to.
isAsterisk <- function(symbol, transform=FALSE, to='.*'){
    symbol <- symbol[1]
    isT <- grepl("*", symbol, fixed = TRUE)
    if(transform){
        if(isT){
            symbol <- gsub("*", to, symbol, fixed = TRUE)
        }
        return(symbol)
    }
    return(isT)
}
#' get cell type column name
#' @noRd
#' @param config config data table
#' @param celltypePattern the pattern of cell type column name
getCelltypeCol <- function(config, celltypePattern='celltype'){
    groupName <- config[config$grp]$ID
    ad <- adist(celltypePattern, groupName, ignore.case = TRUE)[1, ]
    groupName[which.min(ad)][1]
}

#' waffle plot
#' @noRd
#' @param expres expression table returned by `read_exprs`
#' @param id module id
#' @param plotname the name of plotUI
#' @param numGene number of gene
#' @param groupCol group column name, used to compare two groups in the plot
#' @importFrom utils adist
wafflePlot <- function(
        expres, id, plotname, numGene,
        groupCol="treatment"){
    groupValue <- expres[[groupCol]]
    if(all(
        as.character(groupValue)==
        as.character(expres$grpBy), na.rm=TRUE)){
        groupValue <- 1
    }
    list(
        UI = plotLoader(
            plotOutput(
                NS(id, plotname),
                width = '100%',
                height = paste0(min(numGene*max(
                    1, length(unique(groupValue))),
                    max(length(unique(expres$grpBy)), 6)),
                    '00px'))),
        PLOT = renderPlot(scWafflePlot(expres, groupCol))
    )
}
#' get search result by gene name
#' @noRd
#' @param key_words character(1L), gene name
#' @param datafolder the data folder
#' @param auth for locked data
#' @param id namespace
#' @return Html tags for search results
checkGene <- function(
        key_words, datasets,
        auth, global, page=1,
        id, input, output, session){
    exprs <- NULL
    limit <- 5 # return 5 record
    gene <- mappingToSymbols(key_words, transform = TRUE)
    getGeneNamesByKeyword <- function(){
        appconfs <- getAppConf(privilege = auth$privilege)
        gn <- lapply(appconfs, function(.ele){
            if(checkLocker(.ele$id)){
                if(!checkPrivilege(auth$privilege, .ele$id)){
                    return(NULL)
                }
            }
            geneIds <- readData("sc1gene", .ele$id)
            if(any(isQuote(gene))){
                genenames <- geneIds[
                    tolower(names(geneIds)) %in%
                        tolower(removeQuote(gene))]
            }else{
                if(isAsterisk(gene)){
                    gene <- isAsterisk(gene, transform = TRUE)
                    genenames <- geneIds[grepl(
                        gene, names(geneIds), ignore.case = TRUE)]
                    if(length(genenames)>.globals$limitNumGene){
                        genenames <- geneIds[grepl(
                            paste0("^",gene),
                            names(geneIds),
                            ignore.case = TRUE)]
                    }
                }else{
                    genenames <- 
                        geneIds[tolower(names(geneIds)) %in% tolower(gene)]
                }
            }
            if(length(genenames)>0){
                lgs <- listGeneSymbols(genes = names(genenames),
                                       datasets = .ele$id,
                                       checkExpr=TRUE)
                genenames <- genenames[names(genenames) %in% lgs]
            }
            return(genenames)
        })
        keep <- lengths(gn)>0
        return(list(appconfs=appconfs[keep], genenames=gn[keep]))
    }
    tryCatch({
        global <- global()
        if(is.null(global$search_results[[key_words]])){
            showNotification(
                paste('Ploting expression data for multiple datasets.',
                      'It will take a while. Please be patient.'),
                duration = 5,
                type = 'message'
            )
            gn <- getGeneNamesByKeyword()
            global$search_results[[key_words]] <-
                list(
                    genenames= gn$genenames,
                    appconfs = gn$appconfs,
                    page=1,
                    total=ceiling(length(gn$genenames)/limit))
        }
        global$search_results[[key_words]]$page <- page
        from <- limit*(page-1)+1
        to <- min(from+limit-1, length(global$search_results[[key_words]]$genenames))
        if(to>=from){
            genenames <- global$search_results[[key_words]]$genenames[from:to]
            appconfs <- global$search_results[[key_words]]$appconfs[from:to]
            
            exprs <- mapply(function(.appconfs, .genenames){
                .genenames <- .genenames[seq.int(min(
                    length(.genenames),
                    .globals$limitNumGene))]
                config <- readData("sc1conf", .appconfs$id)
                groupCol <- getCelltypeCol(
                    config,
                    celltypePattern =
                        .globals$groupColPattern)
                if(!is.null(.appconfs$groupCol)){
                    if(.appconfs$groupCol!=""){
                        groupCol <- .appconfs$groupCol
                    }
                }
                if(is.null(groupCol)){
                    return(NULL)
                }
                if(groupCol==""){
                    return(NULL)
                }
                ggData <-
                    read_exprs(
                        .appconfs$id,
                        .genenames,
                        readData("sc1meta", .appconfs$id),
                        config, groupCol, valueOnly=FALSE)
                ggData[ggData$val < 0]$val <- 0
                #waffle plot
                plotname <- paste0('search-plot', .appconfs$id)
                wp <- wafflePlot(
                    ggData, id,
                    plotname,
                    length(.genenames),
                    groupCol = groupCol)
                list(
                    UI = tags$li(
                        tags$a(href=paste0(
                            '?data=', .appconfs$id, '&gene=',
                            paste(names(.genenames), collapse=";")),
                            .appconfs$title),
                        wp$UI
                    ),
                    PLOT = wp$PLOT,
                    NAME = plotname
                )
            }, appconfs, genenames, SIMPLIFY = FALSE)
        }
    },
    error = function(e){
        message(e)
    })
    exprs <- exprs[lengths(exprs)>0]
    if(length(exprs)==0){
        return(list(UI=tagList(), PLOT=NULL))
    }else{
        plots <- lapply(exprs, function(.ele){
            .ele$PLOT
        })
        names(plots) <- vapply(exprs, function(.ele) .ele$NAME, character(1L))
        exprs <- lapply(exprs, function(.ele){
            .ele$UI
        })
        if(length(global$evt))
            lapply(global$evt, function(.ele){
                if(!is.null(.ele)){
                    .ele$destroy()
                }
            })
        if(!is.null(global$search_results[[key_words]])){
            if(global$search_results[[key_words]]$total>1){
                lapply(
                    seq.int(global$search_results[[key_words]]$total),
                    function(.id){
                        if(.id!=global$search_results[[key_words]]$page){
                            global$evt[[local({.id})]] <-
                                observeEvent(input[[paste0("page", .id)]],{
                                    updateSearch(
                                        key_words,
                                        datasets=datasets,
                                        auth=auth,
                                        global=reactive({global}),
                                        page = local({.id}),
                                        id=id,
                                        input=input,
                                        output=output,
                                        session=session)
                                }, ignoreInit = TRUE, once = TRUE)
                        }
                    })
            }
        }
        return(
            list(
                UI=tagList(
                    ##pagination
                    div(style="padding-left:2rem;",
                        if(global$search_results[[key_words]]$total>1){
                            lapply(
                                seq.int(global$search_results[[key_words]]$total),
                                function(.id){
                                    if(.id!=global$search_results[[key_words]]$page){
                                        tags$span(actionLink(
                                            NS(id, paste0("page", .id)),
                                            paste("page", .id),
                                            `data-value`=.id))
                                    }else{
                                        tags$em(paste("page", .id))
                                    }
                            })
                        }),
                    tags$ul(exprs)),
                PLOT=plots
            )
        )
    }
}

# vistor plots
updateVisitor <- function(input, output, session){
    updateVisitorTable(input, output, session)
    # conterFilename <- .globals$counterFilename
    # ## update visitor stats
    # update_visitor <- function(){
    #     req(input$remote_addr)
    #     counter <- read.delim(conterFilename, header = TRUE)
    #     ips <- counter$ip
    #     counter <- as.Date(counter$date)
    #     visitors <- paste(format(counter, "%d/%m/%y %H"), ips)
    #     current <- Sys.time()
    #     ip <- isolate(input$remote_addr)
    #     agent <- isolate(input$remote_agent)
    #     if(!paste(format(current, "%d/%m/%y %H"), ip) %in% visitors){
    #         write(
    #             paste(as.character(current), ip, agent, sep="\t"),
    #             conterFilename, append = TRUE)
    #     }
    # }
    # observeEvent(input$remote_addr, update_visitor())
    # output$total_visitor <- renderPlot({
    #     counter <- read.delim(conterFilename, header = TRUE)
    #     counter <- as.Date(counter$date)
    #     counter <- counter[as.numeric(difftime(
    #         as.Date(Sys.time()),
    #         counter,
    #         units = 'days'))<730]
    #     counter <- table(format(counter, "%y-%m"))
    #     counter <- as.data.frame(counter)
    #     ggplot(counter, aes(x=.data[["Var1"]], y=.data[["Freq"]])) +
    #         geom_bar(stat = "identity", fill="darkorchid4") +
    #         theme_minimal() + xlab("") + ylab("visitor counts") +
    #         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    # })
}

# used to avoid suppressWarnings(as.numeric)
char2numeric <- function(x, keep="0-9.-eE+"){
    keep <- grepl(paste0("^[, ", keep, "]+$"), x)
    x[!keep] <- NA
    x <- as.numeric(x)
    x
}

# get assay data by SeuratObject version
#' @importMethodsFrom utils packageVersion
#' @importFrom SeuratObject GetAssayData
extAssayData <- function(object, slot, ...){
    seuObjVersion <- packageVersion('SeuratObject')
    if(seuObjVersion<'5.0.0'){
        GetAssayData(object=object, slot = slot, ...)
    }else{
        GetAssayData(object=object, layer = slot, ...)
    }
}