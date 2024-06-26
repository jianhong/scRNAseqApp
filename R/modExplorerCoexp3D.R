scCoexp3dUI <- function(id, postfix = 1) {
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
        menuUI = contextMenuCoExprUI(id, plotly = TRUE),
        contentUI = tagList(
            uiOutput(NS0(id, "GeneExpr3Doup.ui", 1)),
            downloadButton(
                NS(id, 'downloadExpr'),
                "Expression for clicked cell"),
            verbatimTextOutput(NS(id, 'clicked'))
        )
    )
}
scCoexp3dServer <- function(
        pid,
        id,
        dataSource,
        optCrt,
        p_input,
        p_session,
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
        plot3d <- reactive({
            scDRcoex(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=p_input$GeneExprdrX,
                dimRedY=p_input$GeneExprdrY,
                gene1=input$GeneName1,
                gene2=input$GeneName2,
                subsetCellKey=p_input$subsetCell,
                subsetCellVal=getSubsetCellVal(p_input),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                plotType="3D",
                pointSize=p_input$GeneExprsiz,
                GeneExprDotCol=input$CoExprcol1,
                GeneExprDotOrd=input$CoExprord1,
                labelsFontsize=p_input$GeneExprfsz,
                plotAspectRatio=p_input$GeneExprasp,
                keepXYlables=p_input$GeneExprtxt,
                valueFilterKey = p_input$filterCell,
                valueFilterCutoff = p_input$filterCellVal,
                hideFilterCell = input$CoExprhid1
            )
        })
        source <- NS0(NS(pid, id), "GeneExpr3Doup", 1)
        output$GeneExpr3Doup1 <- renderPlotly({
            plot3d()
        })
        output$GeneExpr3Doup.ui1 <- renderUI({
            plotlyOutput(
                source,
                height = .globals$pList1[p_input$GeneExprpsz])
        })
        output$downloadExpr <- exprDownloadHandler(
            dataSource()$sc1gene,
            dataSource()$dataset,
            dataSource()$sc1meta)
        output$clicked <- plotly3d_click(session)
    })
}
