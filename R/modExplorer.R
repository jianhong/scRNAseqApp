subsetPlotsUI <- function(id) {
    ns <- NS(id)
    tabPanel(
        value = id,
        HTML("Explorer"),
        tabsubTitleUI(
            id,
            'Plots',
            description = paste(
                "In this tab, users can visualise gene expression and",
                "cell information by adding new module."
            )
        ),
        fluidRow(
            column(
                3,
                dimensionReductionUI(id)),
            column(
                5,
                subsetCellByInfoUI(id, ABcolumn=.globals$subsetgroup[1]),
                subsetCellByInfoUI(id, ABcolumn=.globals$subsetgroup[2])
                ),
            column(
                4,
                graphicsControlUI(id),
                subsetCellByFilterUI(id))
        ),
        fluidRow(
            column(
                2,
                selectInput(
                    ns('moduleName'),
                    "Module type",
                    choices = c(
                        'cell info',
                        'gene expression',
                        'peak expression',
                        'proportion',
                        'violin/box plot',
                        'co-expression',
                        'co-expression 3d'
                    ),
                    selected = 'cell info'
                )
            ),
            column(
                2,
                selectInput(
                    ns('moduleWidth'),
                    "Module width",
                    choices = c('half', 'full'),
                    selected = 'half'
                )
            ),
            column(
                2,
                selectInput(
                    ns('interactive'),
                    label = "Interactive",
                    choices = c('No', 'Yes'),
                    selected = 'No'
                )
            ),
            column(
                6,
                div(
                    actionButton(
                        ns("newModule"),
                        "New Module",
                        icon = icon('plus'),
                        class = "align-action-button"
                    ),
                    actionButton(
                        ns("destroyModule"),
                        "Remove All",
                        icon = icon('minus'),
                        class = "align-action-button"
                    ),
                    div(
                        style = 'display: none;',
                        textInput(
                            ns("removePlotModule"),
                            '',
                            value = '',
                            width = 0
                        )),
                    div(
                        style = 'display: none;',
                        textInput(
                            ns("moveupPlotModule"),
                            '',
                            value = '',
                            width = 0
                        )),
                    div(
                        style = 'display: none;',
                        textInput(
                            ns("movedownPlotModule"),
                            '',
                            value = '',
                            width = 0
                        )),
                    div(
                        style = 'display: none;',
                        textInput(
                            ns("resizePlotModule"),
                            '',
                            value = '',
                            width = 0
                        )),
                    div(
                        style = 'display: none;',
                        textInput(
                            ns("changeSubsetContext"),
                            '',
                            value = '',
                            width = 0
                        ))
                ))
        ),
        fluidRow(div(
            id = ns('container'),
            htmlOutput(ns(
                'subplot'
            ))))
    )
}
subsetPlotsServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## subtitle
        output$PlotsSubTitle <-
            renderUI({
                h4(paste(
                    "Explorer",
                    dataSource()$terms['expression'],
                    "on dimension reduction"
                ))
            })
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, dataSource)
        ### Information to show
        
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource,
                           ABcolumns=.globals$subsetgroup)
        
        ## input column 3
        updateFilterCellUI(id, optCrt, input, output, session, dataSource)
        
        ## plot region
        globals <- reactiveValues(
            containerIds = c(),
            containerInteractive = list(),
            containerUIs = list(),
            containerUIsFUN = list(),
            containerWidth = list(),
            containerServers = list(),
            containerSubsetGrp = c(),
            Idpointer = 0
        )
        if (is.null(session$userData$defaults))
            session$userData$defaults <- list()
        if (is.null(session$userData$defaults[[dataSource()$dataset]]))
            session$userData$defaults[[dataSource()$dataset]] <- list()
        uiGrid <- function(ns0, subgrp, uiFUN, width) {
            column(width = width, uiFUN(NS(id, ns0), subgrp=subgrp))
        }
        getRown <- function(w) {
            base <- 0
            currentLine <- 1
            l <- rep(0, length(w))
            for (i in seq_along(w)) {
                if (w[i] == 12) {
                    if (i == 1) {
                        l[i] <- 1
                    } else{
                        l[i] <- l[i - 1] + 1
                    }
                    currentLine <- l[i] + 1
                    base <- 0
                } else{
                    base <- base + w[i]
                    l[i] <- currentLine
                    if (base == 12) {
                        currentLine <- currentLine + 1
                        base <- 0
                    }
                }
            }
            return(l)
        }
        updatePlotModules <- function() {
            coln <- 
                ifelse(
                    globals$containerWidth[globals$containerIds] == "half",
                    6, 12)
            uis <- mapply(
                globals$containerIds,
                globals$containerSubsetGrp[globals$containerIds],
                globals$containerUIsFUN[globals$containerIds],
                coln,
                FUN = uiGrid,
                SIMPLIFY = FALSE)
            names(uis) <- globals$containerIds
            globals$containerUIs <- lapply(split(uis, getRown(coln)), fluidRow)
            output$subplot <- renderUI(globals$containerUIs)
            lapply(globals$containerIds, function(cid) {
                globals$containerServers[[cid]](
                    id,
                    cid,
                    dataSource,
                    optCrt,
                    input,
                    session,
                    globals$containerInteractive[[cid]]
                )
            })
        }
        observeEvent(input$newModule, {
            if(input$moduleName=='peak expression'){
                if(!file.exists(file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames$sc1atac))){
                    showNotification(
                        "Peak expression data not available",
                        duration = 5,
                        closeButton = TRUE,
                        type = "error"
                    )
                    return()
                }
            }
                
            if (length(globals$containerIds) >= 8) {
                showNotification(
                    "Reached the limitation.",
                    duration = 5,
                    closeButton = TRUE,
                    type = "warning"
                )
            } else{
                globals$Idpointer <- globals$Idpointer + 1
                ns0 <- paste0('plot_', globals$Idpointer)
                globals$containerIds <- c(globals$containerIds, ns0)
                globals$containerInteractive[[ns0]] <-
                    isolate(input$interactive == 'Yes')
                globals$containerWidth[[ns0]] <- isolate(input$moduleWidth)
                globals$containerSubsetGrp[ns0] <- .globals$subsetgroup[1]
                globals$containerUIsFUN[[ns0]] <- switch(
                    input$moduleName,
                    "cell info" = scInfoUI,
                    'gene expression' = scExprUI,
                    'proportion' = scPropUI,
                    'violin/box plot' = scVlnUI,
                    'co-expression' = scCoexpUI,
                    'co-expression 3d' = scCoexp3dUI,
                    'peak expression' = scAccUI,
                    scInfoUI
                )
                globals$containerServers[[ns0]] <- switch(
                    input$moduleName,
                    "cell info" = scInfoServer,
                    'gene expression' = scExprServer,
                    'proportion' = scPropServer,
                    'violin/box plot' = scVlnServer,
                    'co-expression' = scCoexpServer,
                    'co-expression 3d' = scCoexp3dServer,
                    'peak expression' = scAccServer,
                    scInfoServer
                )
                updatePlotModules()
            }
        })
        observeEvent(input$destroyModule, {
            lapply(globals$containerUIs, function(ui) {
                removeUI(ui, immediate = TRUE, session = session)
            })
            globals$containerIds <- c()
            globals$containerInteractive <- list()
            globals$containerUIsFUN <- list()
            globals$containerUIs <- list()
            globals$containerWidth <- list()
            globals$containerServers <- list()
            updatePlotModules()
        })
        observeEvent(input$removePlotModule, {
            if (input$removePlotModule != "") {
                globals$containerIds <-
                    globals$containerIds[
                        globals$containerIds !=
                            input$removePlotModule]
                globals$containerInteractive[[input$removePlotModule]] <-
                    NULL
                globals$containerWidth[[input$removePlotModule]] <- NULL
                globals$containerUIsFUN[[input$removePlotModule]] <- NULL
                globals$containerUIs[[input$removePlotModule]] <- NULL
                updatePlotModules()
            }
        })
        observeEvent(input$movedownPlotModule, {
            if (input$movedownPlotModule != "") {
                id <- which(globals$containerIds == input$movedownPlotModule)
                if (length(id) == 1) {
                    if (id != length(globals$containerIds)) {
                        tmp <- globals$containerIds[id + 1]
                        globals$containerIds[id + 1] <-
                            globals$containerIds[id]
                        globals$containerIds[id] <- tmp
                    }
                    updatePlotModules()
                }
            }
        })
        observeEvent(input$moveupPlotModule, {
            if (input$moveupPlotModule != "") {
                id <- which(globals$containerIds == input$moveupPlotModule)
                if (length(id) == 1) {
                    if (id != 1) {
                        tmp <- globals$containerIds[id - 1]
                        globals$containerIds[id - 1] <-
                            globals$containerIds[id]
                        globals$containerIds[id] <- tmp
                    }
                    updatePlotModules()
                }
            }
        })
        observeEvent(input$resizePlotModule, {
            if (input$resizePlotModule != "") {
                globals$containerWidth[[input$resizePlotModule]] <- 
                    ifelse(
                        globals$containerWidth[[input$resizePlotModule]] ==
                            "half",
                        "full",
                        "half")
                updatePlotModules()
            }
        })
        observeEvent(input$changeSubsetContext, {
            if (input$changeSubsetContext != "") {
                globals$containerSubsetGrp[[
                    sub('___.*$', '', input$changeSubsetContext)
                ]] <- sub('^.*___', '', input$changeSubsetContext)
            }
        })
    })
}
