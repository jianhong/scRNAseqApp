scExprUI <- function(id, postfix = 1) {
    subModuleContainerUI(
        id,
        mainSelectUI = selectInput(
            NS0(id, "GeneName", postfix),
            "Gene name:", choices =
                NULL),
        menuUI = contextMenuGeneExprUI(id, postfix, group = TRUE),
        contentUI = geneExprDotPlotUI(id, postfix)
    )
}
scExprServer <- function(
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
        GeneNameLabel <- paste0('GeneName', postfix)
        if (is.null(
            p_session$userData$defaults[[dataSource()$dataset]][[id]])) {
            defaults <- list()
            defaults[[GeneNameLabel]] <- dataSource()$sc1def$gene1
        } else{
            defaults <-
                p_session$userData$defaults[[dataSource()$dataset]][[id]]
        }
        updateSelectizeInput(
            session,
            GeneNameLabel,
            choices = sort(names(dataSource()$sc1gene)),
            server = TRUE,
            selected = defaults[[GeneNameLabel]],
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
                gene1=input[[GeneNameLabel]],
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
                hideFilterCell = input[[paste0("GeneExprhid", postfix)]]
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
            input[[GeneNameLabel]]
        )
    })
}
