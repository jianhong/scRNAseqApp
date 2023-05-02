scVlnUI <- function(id, postfix = 1) {
    subModuleContainerUI(
        id,
        mainSelectUI = tagList(
            selectInput(
                NS(id, "CellInfoX"),
                "X-axis:",
                choices = NULL,
                width = "100px"
            ),
            selectInput(
                NS(id, "CellInfoY"),
                "Y-axis:",
                choices = NULL,
                width = "100px"
            )
        ),
        menuUI = contextMenuViolinUI(id),
        contentUI = geneExprDotPlotUI(id, postfix = postfix)
    )
}
scVlnServer <- function(
        pid,
        id,
        dataSource,
        optCrt,
        p_input,
        p_session,
        interactive,
        postfix = 1) {
    moduleServer(id, function(input, output, session) {
        if (is.null(
            p_session$userData$defaults[[dataSource()$dataset]][[id]])) {
            defaults <- list(
                CellInfoX = dataSource()$sc1def$grp2,
                CellInfoY = dataSource()$sc1def$grp1
            )
        } else{
            defaults <-
                p_session$userData$defaults[[dataSource()$dataset]][[id]]
        }
        ## title
        updateSelectInput(
            session,
            "CellInfoX",
            choices = getGroupUI(dataSource),
            selected = defaults$CellInfoX
        )
        updateSelectizeInput(
            session,
            "CellInfoY",
            server = TRUE,
            choices = c(
                getNonGroupUI(dataSource),
                sort(names(
                    dataSource()$sc1gene
                ))),
            selected =  defaults$CellInfoY,
            options = list(
                maxOptions =
                    length(getNonGroupUI(dataSource)) + 3,
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
            c("CellInfoX", "CellInfoY", "plottyp", "plotpts")
        )
        updateRankList(
            input, output, dataSource, "CellInfoX", "plotXord",
            NS(NS(pid, id), "cellinfoXorder"))
        ## plot
        plot1 <- reactive({
            scVioBox(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$CellInfoX,
                input$CellInfoY,
                p_input$subsetCell,
                p_input$subsetCellVal,
                p_input$filterCell,
                p_input$filterCellVal,
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$plottyp,
                input$plotpts,
                p_input$GeneExprsiz,
                p_input$GeneExprfsz,
                reorder = input$plotord,
                orderX = input$cellinfoXorder
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
            plot1,
            .globals$pList1[p_input$GeneExprpsz],
            dataSource()$dataset,
            input$plottyp,
            input$CellInfoX
        )
    })
}
