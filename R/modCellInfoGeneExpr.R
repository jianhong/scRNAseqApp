cellInfoGeneExprUI <- function(id) {
    tabPanel(
        value = id,
        htmlOutput(NS(id, 'GeneExpr')),
        tabsubTitleUI(
            id,
            'GeneExpr',
            description = paste(
                "In this tab, users can visualise both cell and feature",
                "information side-by-side on low-dimensional representions."
            )
        ),
        fluidRow(
            column(3, dimensionReductionUI(id)),
            column(5, subsetCellByInfoUI(id)),
            column(4, graphicsControlUI(id))
        ),
        fluidRow(
            column(
                6,
                style = "border-right: 2px solid black",
                h4("Cell information"),
                fluidRow(
                    column(6, cellInfoUI(id, 1)),
                    column(6, cellInfoPlotControlUI(id, 1))),
                geneExprDotPlotUI(id, 1, editor=TRUE),
                br(),
                cellInfoTblUI(id, 1)
            ),
            column(
                6,
                htmlOutput(NS0(id, "subPlotTitle", 2)),
                fluidRow(
                    column(6, geneExprUI(id, 2)),
                    column(6, geneExprPlotControlUI(id, 2))),
                geneExprDotPlotUI(id, 2)
            )
        )
    )
}
#' @importFrom DT formatRound renderDT
#' @importFrom magrittr %>%
cellInfoGeneExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## title
        output$GeneExpr <-
            renderUI({
                HTML(paste("CellInfo vs", dataSource()$terms["GeneExpr"]))
            })
        ## subtitle
        output$GeneExprSubTitle <-
            renderUI({
                h4(
                    paste(
                        "Cell information vs gene",
                        dataSource()$terms['expression'],
                        "on reduced dimensions"
                    )
                )
            })
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, dataSource)
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource)
        
        ## plot region
        ### sub region title
        output$subPlotTitle2 <-
            renderUI({
                h4(paste("Gene", dataSource()$terms['expression']))
            })
        ### cellInfo
        updateCellInfoPlot(1, id, input, output, session, dataSource)
        ### expression stats table
        output$GeneExpr.dt1 <- renderDT({
            ggData <- scDRnum(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$CellInfo1,
                input$GeneName2,
                input$subsetCell,
                getSubsetCellVal(input),
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$GeneExprsplt1
            )
            datatable(
                ggData,
                rownames = FALSE,
                extensions = "Buttons",
                options = list(
                    pageLength = -1,
                    dom = "tB",
                    buttons = c("copy", "csv", "excel")
                )
            ) %>%
                formatRound(
                    columns = c("pctExpress"),
                    digits = 2)
        })
        
        ### geneExpr
        selectedGene <- dataSource()$sc1def$gene1
        if (!is.null(dataSource()$genelist)) {
            selectedGene <- dataSource()$genelist[1]
        }
        updateGeneExprPlot(
            postfix = 2,
            selectedGene,
            optCrt,
            id,
            input,
            output,
            session,
            dataSource)
    })
}
