scInfoUI <- function(id, postfix = 1) {
    subModuleContainerUI(
        id,
        mainSelectUI = selectInput(
            NS0(id, "CellInfo", postfix),
            "Cell info:",
            choices = NULL),
        menuUI = contextMenuCellInfoUI(id, postfix),
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
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                p_input$GeneExprdrX,
                p_input$GeneExprdrY,
                input[[cellInfoLabel]],
                p_input$subsetCell,
                getSubsetCellVal(p_input),
                p_input$GeneExprsiz,
                input[[paste0("CellInfocol", postfix)]],
                input[[paste0("CellInfoord", postfix)]],
                p_input$GeneExprfsz,
                p_input$GeneExprasp,
                p_input$GeneExprtxt,
                input[[paste0("CellInfolab", postfix)]],
                dataset = dataSource()$dataset,
                geneIdMap = dataSource()$sc1gene,
                valueFilterKey = p_input$filterCell,
                valueFilterCutoff = p_input$filterCellVal,
                inpSlingshot = input[[paste0("CellInfoslingshot", postfix)]],
                slingshotFilename = file.path(
                    .globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["slingshot"]]
                ),
                interactive = interactive
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
            input[[cellInfoLabel]]
        )
    })
}
