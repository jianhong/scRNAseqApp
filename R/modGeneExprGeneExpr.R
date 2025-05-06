geneExprGeneExprUI <- function(id) {
    tabPanel(
        value = id,
        htmlOutput(NS(id, 'GeneExpr')),
        tabsubTitleUI(
            id,
            'GeneExpr',
            description = paste(
                "In this tab, users can visualise two feature informations ",
                "side-by-side on low-dimensional representions."
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
                htmlOutput(NS0(id, "subPlotTitle", 1)),
                fluidRow(
                    column(6, geneExprUI(id, 1)),
                    column(6, geneExprPlotControlUI(id, 1))),
                geneExprDotPlotUI(id, 1)
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
geneExprGeneExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## title
        output$GeneExpr <-
            renderUI({
                HTML(paste(
                    dataSource()$terms["GeneExpr"],
                    "vs",
                    dataSource()$terms["GeneExpr"]))
            })
        ## subtitle
        output$GeneExprSubTitle <-
            renderUI({
                h4(
                    paste(
                        "Gene",
                        dataSource()$terms['expression'],
                        "vs gene",
                        dataSource()$terms['expression'],
                        "on dimension reduction"
                    )
                )
            })
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, input, dataSource)
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource)
        
        ## plot region
        ### sub region title
        output$subPlotTitle1 <-
            renderUI({
                h4(paste("Gene", dataSource()$terms['expression'], "1"))
            })
        output$subPlotTitle2 <-
            renderUI({
                h4(paste("Gene", dataSource()$terms['expression'], "2"))
            })
        ### gene expressions
        updateGeneExprPlot(
            postfix = 1,
            dataSource()$sc1def$gene1,
            optCrt,
            id,
            input,
            output,
            session,
            dataSource
        )
        updateGeneExprPlot(
            postfix = 2,
            dataSource()$sc1def$gene2,
            optCrt,
            id,
            input,
            output,
            session,
            dataSource
        )
    })
}
