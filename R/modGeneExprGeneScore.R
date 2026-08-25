geneScoreGeneExprUI <- function(id) {
    tabPanel(
        value = id,
        HTML("GeneScore vs GeneExpr"),
        tabsubTitleUI(
            id,
            'ScoreExpr',
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
            resizablePlotContainer(
                leftUI=tagList(
                    htmlOutput(NS0(id, "subPlotTitle", 1)),
                    fluidRow(
                        column(6, geneExprUI(id, 1)),
                        column(6, geneExprPlotControlUI(id, 1, linkXYlim = TRUE))),
                    geneExprDotPlotUI(id, 1)
                ),
                rightUI=tagList(
                    htmlOutput(NS0(id, "subPlotTitle", 2)),
                    fluidRow(
                        column(6, geneExprUI(id, 2)),
                        column(6, geneExprPlotControlUI(id, 2, linkXYlim = TRUE))),
                    geneExprDotPlotUI(id, 2)
                )
            )
        )
    )
}
geneScoreGeneExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## subtitle
        output$ScoreExprSubTitle <-
            renderUI({
                h4(
                    paste(
                        "Gene Score",
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
                h4(paste("Gene Score"))
            })
        output$subPlotTitle2 <-
            renderUI({
                h4(paste("Gene", dataSource()$terms['expression']))
            })
        sharedgene <- intersect(names(dataSource()$sc1gene),
                                names(dataSource()$sc1gsgene))
        if(any(dataSource()$sc1def$gene %in% sharedgene)){
            sharedgene <- dataSource()$sc1def$gene[
                dataSource()$sc1def$gene %in% sharedgene]
        }
        ### gene scores
        updateGeneExprPlot(
            postfix = 1,
            selectedGene = sharedgene[1],
            optCrt = optCrt,
            id = id,
            input = input,
            output = output,
            session = session,
            dataSource = dataSource,
            geneType = 'score'
        )
        ### gene expressions
        updateGeneExprPlot(
            postfix = 2,
            selectedGene = sharedgene[1],
            optCrt = optCrt,
            id = id,
            input = input,
            output = output,
            session = session,
            dataSource = dataSource
        )
    })
}
