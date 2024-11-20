scAccUI <- function(id, postfix = 1, subgrp=.globals$subsetgroup[1]) {
    subModuleContainerUI(
        id,
        mainSelectUI = tagList(
            selectInput(
                NS0(id, "coord", postfix),
                "Coordinates:",
                choices = NULL),
            selectInput(
                NS0(id, "GeneName", postfix),
                "Linked gene name:",
                choices = NULL)
        ),
        menuUI = contextMenuGeneExprUI(id, postfix, group = subgrp),
        contentUI = geneExprDotPlotUI(id, postfix)
    )
}
scAccServer <- function(
        pid,
        id,
        dataSource,
        optCrt,
        p_input,
        p_session,
        interactive,
        postfix = 1) {
    moduleServer(id, function(input, output, session) {
        ## title
        CoordLabel <- paste0('coord', postfix)
        GeneNameLabel <- paste0('GeneName', postfix)
        links <- readData("sc1link", dataSource()$dataset)
        if(length(links$peak)!=length(links)){
            links$peak <- paste(as.character(seqnames(links)),
                                start(links), end(links),
                                sep='-')
        }
        availableTargets <- sort(names(dataSource()$sc1gene))
        if(length(links$gene)){
            availableTargets <- availableTargets[availableTargets %in% 
                                                     links$gene]
        }
        if(length(links$peak)){
            availablePeaks <- sort(links$peak)
        }else{
            if(length(links)){
                availablePeaks <- paste(as.character(seqnames(links)),
                                        start(links), end(links),
                                        sep='-')
            }else{
                availablePeaks <-
                    apply(readData("sc1peak", dataSource()$dataset),
                          1, paste, collapse='-')
            }
        }
        
        if (is.null(
            p_session$userData$defaults[[dataSource()$dataset]][[id]])) {
            defaults <- list()
            defaults[[CoordLabel]] <-
                getCoordByGeneSymbol(dataSource()$sc1def$gene1, links=links)
            defaults[[GeneNameLabel]] <- dataSource()$sc1def$gene1
        } else{
            defaults <-
                p_session$userData$defaults[[dataSource()$dataset]][[id]]
        }
        if(is.null(defaults[[CoordLabel]])){
            defaults[[CoordLabel]] <-
                getCoordByGeneSymbol(dataSource()$sc1def$gene1, links=links)
        }
        if(!is.character(defaults[[CoordLabel]])){
            defaults[[CoordLabel]] <- defaults[[CoordLabel]]$peak[1]
        }
        programChange <- reactiveValues(from='')
        observeEvent(input[[GeneNameLabel]], {
            if(input[[GeneNameLabel]]!='' && programChange$from!='coor'){
                coor <- 
                    getCoordByGeneSymbol(input[[GeneNameLabel]], links=links)
                if(is(coor, 'GRanges')){
                    coor <- coor[coor$peak!='']
                    if(length(coor$peak)){
                        if(length(defaults[[CoordLabel]])){
                            updateCoord <- !defaults[[CoordLabel]] %in%
                                coor$peak
                        }else{
                            updateCoord <- TRUE
                        }
                        if(updateCoord){
                            defaults[[CoordLabel]] <- coor$peak[1]
                            if(defaults[[CoordLabel]]!=input[[CoordLabel]]){
                                programChange$from <- 'gene'
                                updateSelectizeInput(
                                    session,
                                    CoordLabel,
                                    choices = unique(c(coor$peak,
                                                       availablePeaks)),
                                    server = TRUE,
                                    selected = defaults[[CoordLabel]],
                                    options = list(
                                        maxOptions = min(.globals$maxNumGene,
                                                         length(coor)),
                                        create = TRUE,
                                        persist = TRUE,
                                        render = I(optCrt)
                                    )
                                )
                            }
                        }
                        
                    }
                }else{
                    showNotification(
                        paste("No linked peak by givening gene: ",
                              input[[GeneNameLabel]]),
                        duration = 5,
                        closeButton = TRUE,
                        type = "error"
                    )
                }
            }
            if(input[[GeneNameLabel]]!='' && programChange$from=='coor'){
                programChange$from <- ''
            }
        }, ignoreInit = TRUE)
        observeEvent(input[[CoordLabel]], {
            if(input[[CoordLabel]]!='' && programChange$from!='gene'){
                gene <- 
                    getGeneSymbolByCoord(input[[CoordLabel]], links=links)
                if(length(gene)){
                    if(any(gene %in% names(dataSource()$sc1gene))){
                        if(length(defaults[[GeneNameLabel]])){
                            updateGeneSymbol <- !defaults[[GeneNameLabel]] %in%
                                gene
                        }else{
                            updateGeneSymbol <- TRUE
                        }
                        if(updateGeneSymbol){
                            defaults[[GeneNameLabel]] <- 
                                gene[gene %in% names(dataSource()$sc1gene)][1]
                            if(defaults[[GeneNameLabel]]!=input[[GeneNameLabel]]){
                                programChange$from <- 'coor'
                                updateSelectizeInput(
                                    session,
                                    GeneNameLabel,
                                    choices = availableTargets,
                                    server = TRUE,
                                    selected = defaults[[GeneNameLabel]],
                                    options = list(
                                        maxOptions = .globals$maxNumGene,
                                        create = TRUE,
                                        persist = TRUE,
                                        render = I(optCrt)
                                    )
                                )
                            }
                        }
                    }
                }else{
                    showNotification(
                        paste("No linked gene by givening coordinate",
                              input[[CoordLabel]]),
                        duration = 5,
                        closeButton = TRUE,
                        type = "error"
                    )
                }
            }
            if(input[[CoordLabel]]!='' && programChange$from=='gene'){
                programChange$from <- ''
            }
        }, ignoreInit = TRUE)
        updateSelectizeInput(
            session,
            GeneNameLabel,
            choices = availableTargets,
            server = TRUE,
            selected = defaults[[GeneNameLabel]],
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        updateSelectizeInput(
            session,
            CoordLabel,
            choices = availablePeaks,
            server = TRUE,
            selected = if(length(defaults[[CoordLabel]])) defaults[[CoordLabel]] else availablePeaks[1],
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        subModuleMenuObservor(
            id,
            input,
            p_session,
            dataSource,
            c(
                CoordLabel,
                GeneNameLabel,
                paste0("GeneExprcol", postfix),
                paste0("GeneExprord", postfix),
                paste0("GeneExprtype", postfix),
                paste0("GeneExprxlim", postfix),
                paste0("GeneExprrg", postfix)
            )
        )
        ### plots
        plotX <- reactive({
            scDRgene(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=p_input$GeneExprdrX,
                dimRedY=p_input$GeneExprdrY,
                gene1=input[[CoordLabel]],
                subsetCellKey=p_input[[paste0("subsetCell",
                                              input[[paste0("CellInfosubgrp",
                                                            postfix)]])]],
                subsetCellVal=
                    getSubsetCellVal(p_input,
                                     group=input[[paste0("CellInfosubgrp",
                                                         postfix)]]),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                pointSize=p_input$GeneExprsiz,
                gradientCol=input[[paste0("GeneExprcol", postfix)]],
                GeneExprDotOrd=input[[paste0("GeneExprord", postfix)]],
                labelsFontsize=p_input$GeneExprfsz,
                labelsFontFamily=p_input$GeneExprfml,
                plotAspectRatio=p_input$GeneExprasp,
                keepXYlables=p_input$GeneExprtxt,
                inpPlt=input[[paste0("GeneExprtype", postfix)]],
                inpXlim=if (input[[paste0("GeneExprxlimb", postfix)]] %% 2 == 0)
                    0
                else
                    input[[paste0("GeneExprxlim", postfix)]],
                inpColRange =
                    if (input[[paste0("GeneExprrgb", postfix)]] %% 2 == 0)
                        0
                else
                    input[[paste0("GeneExprrg", postfix)]],
                valueFilterKey = p_input$filterCell,
                valueFilterCutoff = p_input$filterCellVal,
                hideFilterCell = input[[paste0("GeneExprhid", postfix)]],
                geneType = 'coor'
            )
        })
        updateSubModulePlotUI(
            postfix,
            pid,
            id,
            input,
            output,
            session,
            interactive,
            plotX,
            .globals$pList1[p_input$GeneExprpsz],
            dataSource()$dataset,
            p_input$GeneExprdrX,
            p_input$GeneExprdrY,
            input[[CoordLabel]]
        )
    })
}
