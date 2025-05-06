geneAccGeneExprUI <- function(id) {
    tabPanel(
        value = id,
        htmlOutput(NS(id, 'AccExpr')),
        tabsubTitleUI(
            id,
            'AccExpr',
            description = paste(
                "In this tab, users can visualise both accessibility and ",
                "feature information side-by-side."
            )
        ),
        fluidRow(
            column(3, dimensionReductionUI(id)),
            column(5, subsetCellByInfoUI(id, multiple=FALSE)),
            column(4, graphicsControlUI(id))
        ),
        fluidRow(
            column(
                6,
                style = "border-right: 2px solid black",
                h4("ATAC track"),
                fluidRow(
                    column(6, geneAccUI(id, 1)),
                    column(6, geneAccPlotControlUI(id, 1))),
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
                    dataSource()$terms["GeneExpr"]))
            })
            ## subtitle
            output$AccExprSubTitle <-
                renderUI({
                    h4(paste(
                        "ATAC vs gene",
                        dataSource()$terms['expression'],
                        "on reduced dimensions"))
                })
            ## input column 1
            ### Dimension Reduction
            updateDimRedSelInputPair(session, input, dataSource)
            ## input column 2
            updateSubsetCellUI(id, input, output, session, dataSource)
            
            ## plot region
            ### sub region title
            output$subPlotTitle2 <-
                renderUI({
                    h4(paste("Gene", dataSource()$terms['expression']))
                })
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
