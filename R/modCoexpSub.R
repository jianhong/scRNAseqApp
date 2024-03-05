scCoexpUI <- function(id, postfix = 1) {
    subModuleContainerUI(
        id,
        mainSelectUI = tagList(
            selectInput(
                NS(id, "GeneName1"),
                "Gene 1:",
                choices = NULL,
                width = "100px"
            ),
            selectInput(
                NS(id, "GeneName2"),
                "Gene 2:",
                choices = NULL,
                width = "100px"
            )
        ),
        menuUI = contextMenuCoExprUI(id),
        contentUI = geneExprDotPlotUI(id, postfix = postfix)
    )
}
scCoexpServer <- function(
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
                GeneName1 = dataSource()$sc1def$gene1,
                GeneName2 = dataSource()$sc1def$gene2
            )
        } else{
            defaults <-
                p_session$userData$defaults[[dataSource()$dataset]][[id]]
        }
        ## title
        updateSelectizeInput(
            session,
            "GeneName1",
            choices = sort(names(dataSource()$sc1gene)),
            server = TRUE,
            selected = defaults$GeneName1,
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        updateSelectizeInput(
            session,
            "GeneName2",
            choices = sort(names(dataSource()$sc1gene)),
            server = TRUE,
            selected = defaults$GeneName2,
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
            c("GeneName1", "GeneName2", "CoExprcol1", "CoExprord1")
        )
        ## plot
        plot1 <- reactive({
            scDRcoex(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                p_input$GeneExprdrX,
                p_input$GeneExprdrY,
                input$GeneName1,
                input$GeneName2,
                p_input$subsetCell,
                getSubsetCellVal(p_input),
                dataSource()$dataset,
                dataSource()$sc1gene,
                "2D",
                p_input$GeneExprsiz,
                input$CoExprcol1,
                input$CoExprord1,
                p_input$GeneExprfsz,
                p_input$GeneExprasp,
                p_input$GeneExprtxt,
                valueFilterKey = p_input$filterCell,
                valueFilterCutoff = p_input$filterCellVal
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
            p_input$GeneExprdrX,
            p_input$GeneExprdrY,
            input$GeneName1,
            input$GeneName2
        )
    })
}
