geneAccGeneExprUI <- function(id) {
    tabPanel(
        value = id,
        htmlOutput(NS(id, 'AccExpr')),
        tabsubTitleUI(
            id,
            'AccExpr',
            description = paste(
                "In this tab, users can visualise both accessbility and ",
                "feature information side-by-side."
            )
        ),
        fluidRow(
            column(3, dimensionReductionUI(id)),
            column(3, subsetCellByInfoUI(id)),
            column(6, graphicsControlUI(id))
        ),
        fluidRow(
            column(
                6,
                style = "border-right: 2px solid black",
                h4("ATAC"),
                fluidRow(
                    column(6, geneAccUI(id, 1)),
                    column(6, geneAccPlotControlUI(id, 1))),
                geneExprDotPlotUI(id, 1)
            ),
            column(
                6,
                h4("Gene expression"),
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
geneAccGeneExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## title
        if(dataSource()$data_types[[dataSource()$dataset]] %in% 
           c("scMultiome", "scATACseq")){
            data_type <- dataSource()$data_types[[dataSource()$dataset]]
            output$AccExpr <- renderUI({
                HTML(paste0(
                    "GeneAcc vs ",
                    .globals$terms[[data_type]]["GeneExpr"]))
            })
            ## subtitle
            output$AccExprSubTitle <-
                renderUI({
                    h4("ATAC vs Expresion on reduced dimensions")
                })
            ## input column 1
            ### Dimension Reduction
            updateDimRedSelInputPair(session, dataSource)
            ## input column 2
            updateSubsetCellUI(id, input, output, session, dataSource)
            
            ## plot region
            selectedGene <- dataSource()$sc1def$gene1
            ### geneAcc
            updateGeneAccPlot(
                postfix = 1,
                genePostfix = 2,
                optCrt,
                id,
                input,
                output,
                session,
                dataSource)
            
            ### geneExpr
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
        }else{
            output$AccExpr <- renderUI({
                HTML("")
            })
        }
    })
}
