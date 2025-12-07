scVlnUI <- function(id, postfix = 1, subgrp=.globals$subsetgroup[1], ...) {
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
        menuUI = contextMenuViolinUI(id, group = subgrp),
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
        plotX <- reactive({
            scVioBox(
                inpConf = dataSource()$sc1conf,
                inpMeta = dataSource()$sc1meta,
                infoX = input$CellInfoX,
                infoY = input$CellInfoY,
                subsetCellKey=p_input[[paste0("subsetCell",
                                              input[[paste0("CellInfosubgrp",
                                                            postfix)]])]],
                subsetCellVal=
                    getSubsetCellVal(p_input,
                                     group=input[[paste0("CellInfosubgrp",
                                                         postfix)]]),
                valueFilterKey = p_input$filterCell,
                valueFilterCutoff = p_input$filterCellVal,
                valueFilterCutoff2 = p_input$filterCellVal2,
                dataset = dataSource()$dataset,
                geneIdMap = dataSource()$sc1gene,
                inptyp = input$plottyp,
                inppts = input$plotpts,
                pointSize = p_input$GeneExprsiz,
                labelsFontsize = p_input$GeneExprfsz,
                labelsFontFamily=p_input$GeneExprfml,
                reorder = input$plotord,
                orderX = input$cellinfoXorder,
                addnoise = input$addnoise,
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
            lasso = FALSE,
            dataSource()$dataset,
            input$plottyp,
            input$CellInfoX
        )
    })
}
