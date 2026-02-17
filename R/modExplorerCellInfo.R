scInfoUI <- function(id, postfix = 1,
                     subgrp=.globals$subsetgroup[1],
                     coorgrp=.globals$subsetgroup[1]) {
    subModuleContainerUI(
        id,
        mainSelectUI = selectInput(
            NS0(id, "CellInfo", postfix),
            "Cell info:",
            choices = NULL),
        menuUI = contextMenuCellInfoUI(id, postfix,
                                       group=subgrp, coorgrp=coorgrp),
        contentUI = geneExprDotPlotUI(id, postfix)
    )
}
scInfoServer <- function(
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
        cellInfoLabel <- paste0('CellInfo', postfix)
        if (is.null(
            p_session$userData$defaults[[dataSource()$dataset]][[id]])) {
            defaults <- list()
            defaults[[cellInfoLabel]] <- dataSource()$sc1def$meta1
        } else{
            defaults <-
                p_session$userData$defaults[[dataSource()$dataset]][[id]]
        }
        updateSelectInput(
            session,
            cellInfoLabel,
            "Cell info:",
            choices = dataSource()$sc1conf$UI,
            selected = defaults[[cellInfoLabel]]
        )
        output[[paste0('subsetCellNum', postfix)]] <- 
            renderText(paste('% of', nrow(dataSource()$sc1meta), 'cells'))
        observeEvent({
            getSubsetCellVal(p_input,
                             group=input[[paste0("CellInfosubgrp",
                                                 postfix)]])
            },{
            output[[paste0('subsetCellNum', postfix)]] <- 
                renderText(paste('% of', getFilteredCellNum(
                    inpConf=dataSource()$sc1conf,
                    inpMeta=dataSource()$sc1meta,
                    dimRedX=p_input[[paste0("GeneExprdrX",
                                            input[[paste0("CellInfoCoor",
                                                          postfix)]])]],
                    dimRedY=p_input[[paste0("GeneExprdrY",
                                            input[[paste0("CellInfoCoor",
                                                          postfix)]])]],
                    cellinfoID=input[[cellInfoLabel]],
                    subsetCellKey=
                        p_input[[paste0("subsetCell",
                                        input[[paste0("CellInfosubgrp",
                                                      postfix)]])]],,
                    subsetCellVal=
                        getSubsetCellVal(p_input,
                                         group=input[[paste0("CellInfosubgrp",
                                                             postfix)]]),
                    subsetCellPct=100,
                    dataset = dataSource()$dataset,
                    geneIdMap = dataSource()$sc1gene,
                    valueFilterKey = p_input$filterCell,
                    valueFilterCutoff = p_input$filterCellVal,
                    valueFilterCutoff2 = p_input$filterCellVal2,
                    interactive = interactive,
                    selectedCellIDs = p_session$userData$selectedCellIDs
                ), 'cells'))
        })
        subModuleMenuObservor(
            id,
            input,
            p_session,
            dataSource,
            c(
                cellInfoLabel,
                paste0("CellInfocol", postfix),
                paste0("CellInfoord", postfix),
                paste0("CellInfolab", postfix),
                paste0("CellInfoslingshot", postfix)
            )
        )
        ## plot
        plotX <- reactive({
            scDRcell(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=p_input[[paste0("GeneExprdrX",
                                         input[[paste0("CellInfoCoor",
                                                       postfix)]])]],
                dimRedY=p_input[[paste0("GeneExprdrY",
                                               input[[paste0("CellInfoCoor",
                                                             postfix)]])]],
                cellinfoID=input[[cellInfoLabel]],
                subsetCellKey=p_input[[paste0("subsetCell",
                                              input[[paste0("CellInfosubgrp",
                                                            postfix)]])]],
                subsetCellVal=
                    getSubsetCellVal(p_input,
                                     group=input[[paste0("CellInfosubgrp",
                                                         postfix)]]),
                subsetCellPct = input[[paste0("subsetCellPct", postfix)]],
                pointSize=p_input$GeneExprsiz,
                gradientCol=input[[paste0("CellInfocol", postfix)]],
                GeneExprDotOrd=input[[paste0("CellInfoord", postfix)]],
                labelsFontsize=p_input$GeneExprfsz,
                labelsFontFamily=p_input$GeneExprfml,
                plotAspectRatio=p_input$GeneExprasp,
                keepXYlables=p_input$GeneExprtxt,
                inplab=input[[paste0("CellInfolab", postfix)]],
                dataset = dataSource()$dataset,
                geneIdMap = dataSource()$sc1gene,
                valueFilterKey = p_input$filterCell,
                valueFilterCutoff = p_input$filterCellVal,
                valueFilterCutoff2 = p_input$filterCellVal2,
                hideFilterCell=input[[paste0("CellInfohid", postfix)]],
                inpSlingshot = input[[paste0("CellInfoslingshot", postfix)]],
                slingshotFilename = file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["slingshot"]]
                ),
                inpShowEdge = input[[paste0("CellInfoedge", postfix)]],
                edgeFilename = file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["sc1edge"]]
                ),
                inpCellBorder=input[[paste0('CellInfoSegmentation', postfix)]],
                cellborderFilename=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["cellborder"]]),
                cellSegAlpha = input[[paste0('CellInfoSegAlpha', postfix)]],
                inpBgImg=input[[paste0('CellInfoBgImg', postfix)]],
                backgroundImage=file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["backgroundImage"]]),
                interactive = interactive,
                selectedCellIDs = p_session$userData$selectedCellIDs
            )
        })
        updateSubModulePlotUI(
            postfix = postfix,
            pid = pid,
            id = id,
            input = input,
            output = output,
            session = session,
            p_session = p_session,
            interactive = interactive,
            plotX = plotX,
            height = .globals$pList1[p_input$GeneExprpsz],
            lasso = TRUE,
            dataSource()$dataset,
            p_input$GeneExprdrX,
            p_input$GeneExprdrY,
            input[[cellInfoLabel]]
        )
    })
}
