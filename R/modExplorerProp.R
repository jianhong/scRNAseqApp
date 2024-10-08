scPropUI <- function(id, postfix = 1, subgrp=.globals$subsetgroup[1]) {
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
        menuUI = contextMenuPropUI(id, group=subgrp),
        contentUI = geneExprDotPlotUI(id, postfix = postfix)
    )
}
scPropServer <- function(
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
        ## input column
        updateSelectInput(
            session,
            "CellInfoX",
            choices = getGroupUI(dataSource),
            selected = defaults$CellInfoX
        )
        updateSelectInput(
            session,
            "CellInfoY",
            choices = getGroupUI(dataSource),
            selected = defaults$CellInfoY
        )
        updateRankList(
            input, output, dataSource, "CellInfoX", "plotXord",
            NS(NS(pid, id), "cellinfoXorder"))
        updateRankList(
            input, output, dataSource, "CellInfoY", "plotYord",
            NS(NS(pid, id), "cellinfoYorder"))
        
        subModuleMenuObservor(
            id,
            input,
            p_session,
            dataSource,
            c("CellInfoX", "CellInfoY", "plottyp", "plotflp")
        )
        
        ## plot region
        ### plots
        plot1 <- reactive({
            scProp(
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
                inptyp = input$plottyp,
                flipXY = input$plotflp,
                labelsFontsize = p_input$GeneExprfsz,
                labelsFontFamily=p_input$GeneExprfml,
                dataset = dataSource()$dataset,
                geneIdMap = dataSource()$sc1gene,
                valueFilterKey = p_input$filterCell,
                valueFilterCutoff = p_input$filterCellVal,
                reorder = input$plotord,
                orderX = input$cellinfoXorder,
                orderY = input$cellinfoYorder
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
            input$CellInfoX,
            input$CellInfoY
        )
    })
}
