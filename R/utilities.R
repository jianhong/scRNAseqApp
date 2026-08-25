#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @importFrom methods getPackageName
visitorDependencies <- function(){
    htmlDependency(
        name = "scRNAseqApp-assets",
        version = packageVersion(getPackageName()),
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
        }else{
            if (!is.null(query[['data']])) {
                defaultDataset <- query[['data']]
                from <- 'data'
            }
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
        border = "left",
        info = '',
        details = '',
        id = NULL) {
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
                            title
                        ),
                        div(
                            class = "h5 mb-0 font-weight-bold text-gray-800",
                            if(!is.null(id)){
                                textOutput(NS(id, id=paste0('stats', title)),
                                       inline = TRUE)
                            }else{
                                value
                            }
                        )
                    ),
                    div(
                        class = "col-auto about-large-icon about-right",
                        icon(class=paste0("about-", style), icon)
                    )
                )
            )
        ),
        info = info,
        details = details
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

getLogToken <- function(session){
    query <- getQueryString(session = session)
    if(!is.null(query$token)){
        gsub('\"', "", query$token)
    } else {
        NULL
    }
}
# update search results
#' @importFrom utils combn
updateSearch <- function(
        key_words, datasets,
        auth, global, page=1,
        id, input, output, session){
    updateCheckboxInput(session=session,
                        inputId='s_res_flag',
                        value = FALSE)
    on.exit({
        updateCheckboxInput(session=session,
                            inputId='s_res_flag',
                            value = TRUE)
    })
    if(key_words %in% names(getTokenList())){## is token
        token <- key_words
        dataset <- getTokenList()[[token]]
        datasets <- listDatasets(privilege='all', named=TRUE)
        output$search_res <- renderUI(
            tags$ul(
                class='about-ul',
                tags$li(
                    tags$a(
                        href=paste0('?token=', token),
                        names(datasets)[datasets==dataset])
                )
            )
        )
        return(invisible())
    }
    updateSearchTable(key_words)##record the search key
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
        key_words <- unique(key_words)
        if(length(key_words)>1){
            key_words <- lapply(seq.int(length(key_words)), function(m){
                cmb <- combn(key_words, m, simplify = FALSE)
                lapply(cmb, paste, collapse = " ")
            })
            key_words <- unlist(key_words)
        }
        
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
                        # add token if logged
                        token <- getLogToken(session = session)
                        if(!is.null(token)){
                            token <- paste0('&token=', token)
                        }
                        return(tags$li(
                            tags$a(
                                href=paste0('?data=', .ele[1], token),
                                .ele[2],
                                onclick = 
                                    paste0('event.preventDefault();',
                                    'Shiny.onInputChange(',
                                    '\"search_results_select_button\",  \"',
                                    .ele[1],'\");window.scrollTo(0, 0);'))
                            ))
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
            gn <- getGeneNamesByKeyword()
            if(length(gn$genenames)>0){
                showNotification(
                    paste('Plotting expression data for multiple datasets.',
                          'It will take a while. Please be patient.'),
                    duration = 5,
                    type = 'message'
                )
            }
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
                            .appconfs$title, 
                            onclick = 
                                paste0('event.preventDefault();',
                                       'Shiny.onInputChange(',
                                       '\"search_results_select_button\",  \"',
                                       .appconfs$id,
                                       '\");window.scrollTo(0, 0);')),
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
}

# used to avoid suppressWarnings(as.numeric)
char2numeric <- function(x, keep="[., 0-9eE+-]"){
    keep <- grepl(paste0("^", keep, "+$"), x)
    x[!keep] <- NA
    x <- gsub('[ ,]+', '', x)
    x <- gsub('eminus', 'e-', gsub('-', '', gsub('e-', 'eminus', x)))
    x <- as.numeric(x)
    x
}

# get assay data by SeuratObject version
#' @importFrom utils packageVersion
#' @importFrom SeuratObject GetAssayData
extAssayData <- function(object, slot, ...){
    seuObjVersion <- packageVersion('SeuratObject')
    if(seuObjVersion<'5.0.0'){
        GetAssayData(object=object, slot = slot, ...)
    }else{
        GetAssayData(object=object, layer = slot, ...)
    }
}

# load config file
loadConfigFile <- function(.globals, app_path){
    configFile <- file.path(app_path, 'www/config.dcf')
    if(file.exists(configFile)){
        configs <- read.dcf(configFile)
        cn <- intersect(colnames(configs), names(.globals))
        for(i in cn){
            if(i=='theme'){
                theme <- eval(parse(text=paste0("bs_theme(", configs[1, i], ")")))
                .globals$theme <- theme
            }else{
                if(!is.list(.globals[[i]])){
                    param <- strsplit(configs[1, i], ",\\s*")[[1]]
                    mode(param) <- mode(.globals[[i]])
                    .globals[[i]] <- param
                }
            }
        }
    }else{
        configFile <- file.path(app_path, 'www/config.rds')
        ## no safety check here, need to reconsider.
        if(file.exists(configFile)){
            configs <- readRDS(configFile)
            cn <- intersect(names(configs), names(.globals))
            for(i in cn){
                if(is.list(.globals[[i]])){
                    cn2 <- intersect(names(.globals[[i]]), names(configs[[i]]))
                    for(j in cn2){
                        .globals[[i]][[j]] <- configs[[i]][[j]]
                    }
                }else{
                    .globals[[i]] <- configs[[i]]
                }
            }
        }
    }
    return(.globals)
}

#' @importFrom grDevices col2rgb
#' @importFrom ggplot2 theme
is_dark <- function(color) {
    rgb <- col2rgb(color)
    luminance <- 0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]
    return(luminance < 128)
}
get_current_bg <- function(dataSource){
    theme <- bs_current_theme()
    if(!missing(dataSource) && is.function(dataSource)){
        if(isTRUE(dataSource()$theme_mode=="dark")){
            return(bslib::bs_get_variables(theme, 'body-bg-dark'))
        }
    }
    return(bslib::bs_get_variables(theme, "body-bg"))
}
#' @importFrom bslib bs_get_variables bs_current_theme
darkTheme <- function(p, returnBG=FALSE, dataSource, bg_color){
    # Extract the background color
    if(missing(bg_color)||is.null(bg_color)){
        bg_color <- get_current_bg(dataSource=dataSource)
    }
    if(returnBG) return(bg_color)
    if(isTRUE(is_dark(bg_color))){
        ds <- if (!missing(dataSource) && is.function(dataSource)) dataSource() else NULL
        required_keys <- c("bg_color", "panel_bg", 'panel_border',
                           "grid_color", "grid_type", "grid_size",
                           "axis_text", "axis_title",
                           "legend_text", "legend_bg", "legend_title",
                           "strip_bg", "strip_text")
        use_dark <- isTRUE(dataSource()$theme_mode=="dark") && 
            all(required_keys %in% names(ds$dark_theme))
        if(is(p, 'ggplot')){
            darktheme <- theme(
                plot.background = element_rect(fill = bg_color, color = NA),
                panel.background = element_rect(fill = "gray20", color = NA),
                panel.border = element_blank(),
                panel.grid = element_line(color = "gray30"),
                axis.text = element_text(color = "white"),
                axis.title = element_text(color = "white"),
                plot.title = element_text(color = "white"),
                plot.subtitle = element_text(color = "white"),
                legend.background = element_rect(fill = bg_color, color = NA),
                legend.text = element_text(color = "white"),
                legend.title = element_text(color = "white"),
                legend.key = element_rect(fill = bg_color, color = NA),
                strip.background = element_rect(fill = "gray20", color = NA),
                strip.text = element_text(color = "white")
            )
            if(use_dark){
                darktheme <- theme(
                    plot.background = element_rect(fill = ds$dark_theme$bg_color,
                                                   color = NA),
                    panel.background = element_rect(fill = ds$dark_theme$panel_bg,
                                                    color = NA),
                    panel.border = if (ds$dark_theme$panel_border == "blank") {
                        element_blank()
                    } else {
                        element_rect(fill = NA, color = ds$dark_theme$grid_color)
                    },
                    panel.grid = switch(ds$dark_theme$grid_type,
                                        both  = element_line(
                                            color = ds$dark_theme$grid_color, 
                                            linewidth = ds$dark_theme$grid_size),
                                        major = element_line(
                                            color = ds$dark_theme$grid_color,
                                            linewidth = ds$dark_theme$grid_size),
                                        none  = element_blank()
                    ),
                    axis.text = element_text(
                        color = ds$dark_theme$axis_text),
                    axis.title = element_text(
                        color = ds$dark_theme$axis_title),
                    plot.title = element_text(
                        color = ds$dark_theme$plot_title),
                    legend.background = 
                        element_rect(fill = ds$dark_theme$legend_bg, color = NA),
                    legend.text = 
                        element_text(color = ds$dark_theme$legend_text),
                    legend.title = 
                        element_text(color = ds$dark_theme$legend_title),
                    legend.key = 
                        element_rect(fill = ds$dark_theme$legend_bg, color = NA),
                    strip.background = 
                        element_rect(fill = ds$dark_theme$strip_bg, color = NA),
                    strip.text = element_text(color = ds$dark_theme$strip_text)
                )
            }
            return(p + darktheme)
        }
        if(is(p, 'plotly')){
            if(use_dark){
                return(layout(p,
                    paper_bgcolor = ds$dark_theme$panel_bg, 
                    plot_bgcolor = ds$dark_theme$bg_color,
                    font = list(color = ds$dark_theme$legend_text),
                    scene =list(
                    xaxis = list(
                        gridcolor = ds$dark_theme$grid_color,
                        zerolinecolor = ds$dark_theme$axis_text,
                        linecolor = ds$dark_theme$axis_text,
                        gridwidth = ds$dark_theme$grid_size,
                        zerolinewidth = 2 * ds$dark_theme$grid_size
                    ),
                    yaxis = list(
                        gridcolor = ds$dark_theme$grid_color,
                        zerolinecolor = ds$dark_theme$axis_text,
                        linecolor = ds$dark_theme$axis_text,
                        gridwidth = ds$dark_theme$grid_size,
                        zerolinewidth = 2 * ds$dark_theme$grid_size
                    ),
                    zaxis = list(
                        gridcolor = ds$dark_theme$grid_color,
                        zerolinecolor = ds$dark_theme$axis_text,
                        linecolor = ds$dark_theme$axis_text,
                        gridwidth = ds$dark_theme$grid_size,
                        zerolinewidth = 2 * ds$dark_theme$grid_size
                    )
                    ),
                    legend = list(
                        bgcolor=ds$dark_theme$legend_bg
                    )
                ))
            }
            return(layout(p,
                          paper_bgcolor = "#1a1a2e",
                          plot_bgcolor = "#16213e",
                          font = list(color = "#ffffff"),
                          scene =list(
                              xaxis = list(
                                  gridcolor = "#2d3561",
                                  zerolinecolor = "#eaeaea",
                                  linecolor = "#eaeaea"
                              ),
                              yaxis = list(
                                  gridcolor = "#2d3561",
                                  zerolinecolor = "#eaeaea",
                                  linecolor = "#eaeaea"
                              ),
                              zaxis = list(
                                  gridcolor = "#2d3561",
                                  zerolinecolor = "#eaeaea",
                                  linecolor = "#eaeaea"
                              )
                          ),
                          legend = list(
                              bgcolor = "#1a1a2e",
                              bordercolor = "#2d3561"
                          )))
        }  
    }
    return(p)
}

#' @importFrom ggplot2 xlim ylim ggplot_build ggplot_gtable layer_scales coord_cartesian coord_fixed
#' @importFrom grid convertWidth
addLimits <- function(p, ranges=NULL, coord=NULL, id, postfix, input, ...){
    notify <- FALSE
    if(is(p, 'ggplot')){
        if(!is.null(coord)){
            ## ATAC plot
            if(!is.null(ranges)){
                try({
                    x <- ranges$x
                    gr0 <- GRanges(coord)
                    gr <- c(start(gr0), end(gr0))
                    infoLabel <- paste0('GeneExpext.info', postfix)
                    savedCoord <- isolate({
                        input[[infoLabel]]
                    })
                    if(savedCoord!=''){
                        gr <- as.numeric(strsplit(savedCoord, '-')[[1]])
                    }
                    gt <- ggplot_gtable(ggplot_build(p[[1]]))
                    w <- convertWidth(gt$widths, 'npc', valueOnly=TRUE)
                    ## the plot region is in grid 7
                    left <- sum(w[seq.int(6)])
                    right <- sum(w[-seq.int(7)])
                    ## remove all the left and right part from the xlim
                    x <- (x-left)/(1-right-left)
                    x[x<0] <- 0
                    x[x>1] <- 1
                    x <- x*(gr[2]-gr[1]) + gr[1]
                    x <- round(x)
                    p[[1]] <- p[[1]] + xlim(x) ## tracks
                    p[[2]] <- p[[2]] + xlim(x) ## annotations
                    updateTextInput(inputId = infoLabel,
                                    value = paste(x, collapse = '-'))
                    notify <- TRUE
                })
            }
        }else{
            if(!is.null(ranges)){
                ## for background image, filter the plot data by x, and y
                x <- ranges$x
                y <- ranges$y
                ratio <- NULL
                if(!is.null(p$meta$fixCoord)){
                    if (isTRUE(p$meta$fixCoord$aspectRatio == "Square")) {
                        ratio  <- p$meta$fixCoord$ratio
                    } else if (isTRUE(p$meta$fixCoord$aspectRatio == "Fixed")) {
                        ratio  <- 1
                    }
                }
                if('geom_raster' %in% names(p$layers)){
                    raster_layer <- p$layers[['geom_raster']]
                    expandX <- diff(x) * 0.05
                    expandY <- diff(y) * 0.05
                    if(all(c('x', 'y') %in% colnames(raster_layer$data))){
                        if(is.null(y)){
                            raster_layer$data <- 
                                raster_layer$data[
                                    raster_layer$data$x >= x[1] - expandX &
                                        raster_layer$data$x <= x[2] + expandX, ,
                                    drop=FALSE]
                        }else{
                            if(is.null(x)){
                                raster_layer$data <- 
                                    raster_layer$data[
                                        raster_layer$data$y >= y[1] - expandY &
                                            raster_layer$data$y <=
                                            y[2] + expandY, ,
                                        drop=FALSE]
                            }else{
                                raster_layer$data <- 
                                    raster_layer$data[
                                        raster_layer$data$x >= x[1] - expandX &
                                            raster_layer$data$x <=x[2]+expandX &
                                            raster_layer$data$y >=y[1]-expandY &
                                            raster_layer$data$y <=y[2]+expandY,
                                        , drop=FALSE]
                            }
                        }
                        p$layers[['geom_raster']] <- raster_layer
                    }
                }
                if(is.null(y)){
                    if(is(layer_scales(p)$x, 'ScaleContinuousPosition')){
                        p <- p + coord_fixed(xlim = x,
                                                 ratio=ratio)
                        notify <- TRUE
                    }
                }else{
                    if(is.null(x)){
                        if(is(layer_scales(p)$y, 'ScaleContinuousPosition')){
                            p <- p + coord_fixed(ylim = y,
                                                     ratio=ratio)
                            notify <- TRUE
                        }
                    }else{
                        if(is(layer_scales(p)$x, 'ScaleContinuousPosition') &&
                           is(layer_scales(p)$y, 'ScaleContinuousPosition'))
                            p <- p + coord_fixed(xlim = x,
                                                     ylim = y,
                                                     ratio=ratio)
                        notify <- TRUE
                    }
                }
            }
        }
    }
    if(notify){
        removeNotification('cancelZoomIn') # avoid multiple notification
        showNotification("Double click to cancel zoom in.",
                         type = 'message', id='cancelZoomIn')
    }
    return(p)
}

# rotate, scale and shift the cell borders to fit the image
transformImage <- function(
        plot_data,
        scaleX, scaleY,
        offsetX, offsetY,
        Rotation, img_height, ...){
    if(missing(scaleX) || 
       missing(scaleY) ||
       missing(offsetX) ||
       missing(offsetY) ||
       missing(Rotation) ||
       missing(img_height)){
        return(plot_data)
    }
    if(!is.numeric(scaleX) ||
       !is.numeric(scaleY) ||
       !is.numeric(offsetX) ||
       !is.numeric(offsetY) ||
       !is.numeric(Rotation) ||
       !is.numeric(img_height) ){
        return(plot_data)
    }
    
    if(all(c('X', 'Y') %in% toupper(colnames(plot_data)))){
        x <- colnames(plot_data)[toupper(colnames(plot_data))=='X']
        y <- colnames(plot_data)[toupper(colnames(plot_data))=='Y']
    }else{
        return(plot_data)
    }
    theta <- Rotation * pi / 180

    plot_data[[x]] <- (plot_data[[x]]-offsetX)*scaleY
    plot_data[[y]] <- (plot_data[[y]]-offsetY)*scaleX
    rot_X <- 0
    rot_Y <- img_height
    plot_data[[x]] <- (plot_data[[x]]-rot_X) * cos(theta) - 
        (plot_data[[y]]-rot_Y) * sin(theta) + rot_X
    plot_data[[y]] <- (plot_data[[x]]-rot_X) * sin(theta) + 
        (plot_data[[y]]-rot_Y) * cos(theta) + rot_Y
    if(all(c('xend', 'yend') %in% tolower(colnames(plot_data)))){
        xend <- colnames(plot_data)[tolower(colnames(plot_data))=='xend']
        yend <- colnames(plot_data)[tolower(colnames(plot_data))=='yend']
        plot_data[[xend]] <- (plot_data[[xend]]-offsetX)*scaleY
        plot_data[[yend]] <- (plot_data[[yend]]-offsetY)*scaleX
        plot_data[[xend]] <- (plot_data[[xend]]-rot_X) * cos(theta) - 
            (plot_data[[yend]]-rot_Y) * sin(theta) + rot_X
        plot_data[[yend]] <- (plot_data[[xend]]-rot_X) * sin(theta) + 
            (plot_data[[yend]]-rot_Y) * cos(theta) + rot_Y
    }
    return(plot_data)
}

checkCellSegmentationAvailability <- 
    function(env){
    if(isTRUE(env$inpCellBorder)){
        if (file.exists(env$cellborderFilename)) {
            env$cellborder <- readRDS(env$cellborderFilename)
            # cellborderFilename must be a RDS filename.
            # The RDS file must be a list of cell borders.
            # The names of the list is the reductions name, such as umap
            if(is.list(env$cellborder)){
                dimRed_prefix <- sub('.$', '', env$dimRedX)
                if(dimRed_prefix==sub('.$', '', env$dimRedY)){
                    if(dimRed_prefix %in% names(env$cellborder)){
                        env$cellborder <- env$cellborder[[dimRed_prefix]]
                        if(!all(c('x', 'y', 'idx', 'sampleID') %in%
                                colnames(env$cellborder))){
                            showNotification(
                                "The cell border should be a table with
                                x, y, idx, sampleID",
                                type = 'error',
                                duration = 5)
                            env$inpCellBorder <- FALSE
                        }else{
                            env$ggData$sampleID <- env$inpMeta$sampleID
                        }
                    }else{
                        env$inpCellBorder <- FALSE
                    }
                }else{
                    env$inpCellBorder <- FALSE
                }
            }else{
                env$inpCellBorder <- FALSE
            }
        }else{
            env$inpCellBorder <- FALSE
        }
    }
}

checkBgImgAvailability <- 
    function(env){
        backgroundAlignFun <- NULL
        if(isTRUE(env$inpBgImg)){
            if(file.exists(env$backgroundImage)){
                env$backgroundImage <- readRDS(env$backgroundImage)
                if(is.list(env$backgroundImage)){
                    dimRed_prefix <- sub('.$', '', env$dimRedX)
                    if(dimRed_prefix==sub('.$', '', env$dimRedY)){
                        if(dimRed_prefix %in% names(env$backgroundImage)){
                            env$backgroundImage <- env$backgroundImage[[dimRed_prefix]]
                            backgroundAlignFun <- 
                                env$backgroundImage$FUN
                            if(is.null(backgroundAlignFun)){
                                backgroundAlignFun <- transformImage
                            }
                            env$backgroundAlignArgs <-
                                env$backgroundImage$args
                            env$backgroundImage <- env$backgroundImage$raster_df
                            if(!all(c('x', 'y', 'value') %in% 
                                    colnames(env$backgroundImage))){
                                showNotification(
                                    "The background should be a table with
                                x, y, value",
                                    type = 'error',
                                    duration = 5)
                                env$inpBgImg <- FALSE
                            }
                        }else{
                            env$inpBgImg <- FALSE
                        }
                    }else{
                        env$inpBgImg <- FALSE
                    }
                }else{
                    env$inpBgImg <- FALSE
                }
            }else{
                env$inpBgImg <- FALSE
            }
        }
        return(backgroundAlignFun)
    }

updateLimRange <- function(postfix, input, session, dataSource, limid, X=TRUE, val){
    if(isTRUE(X)){
        dimred <- input$GeneExprdrX
        label <- 'Xlim range:'
        manuID <- 'X'
    }else{
        dimred <- input$GeneExprdrY
        label <- 'Ylim range:'
        manuID <- 'Y'
    }
    if(isTRUE(dimred %in% dataSource()$sc1conf$UI)){
        if(missing(val)){
            id <- which(dataSource()$sc1conf$UI == dimred)
            id <- dataSource()$sc1conf[id, ]$ID
            if(length(id)){
                val <- dataSource()$sc1meta[[id[1]]]
            }
        }
        
        if(length(val)){
            minv <- floor(min(val, na.rm = TRUE))
            maxv <- getM(val)
            stepv <- (maxv-minv)/100
            if(!is.na(minv) && !is.na(maxv)){
                updateSliderInput(
                    session,
                    limid,
                    label = label,
                    min = minv-100*stepv,
                    max = maxv+100*stepv,
                    value = c(minv-5*stepv, maxv+5*stepv),
                    step = stepv
                )
            }
            updateNumericInput(
                inputId = paste0('manu', manuID, 'limOriMin', postfix),
                value = minv-5*stepv)
            updateNumericInput(
                inputId = paste0('manu', manuID, 'limOriMax', postfix),
                value = maxv+5*stepv)
        }
    }
}
