plotWaffleUI <- function(id) {
    tabPanel(
        value = id,
        HTML("Waffle Plot"),
        h4("Gene expression waffle plot"),
        "In this tab, users can visualise the gene expression by waffle plot",
        "across groups of cells (e.g. libary / clusters).",
        "Each cell in waffle plot represents the expression mean value of 1% ",
        "number of the maximal group cells",
        br(),
        br(),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                textAreaInput(
                    NS(id, "genelist"),
                    HTML(
                        "List of gene names <br />
                            (Max 3 genes (over 3 will be very slow), <br />
                            separated by , or ; or newline):"
                    ),
                    height = "100px",
                    value = NULL
                ),
                xaxisCellInfoUI(id),
                yaxisCellInfoUI(id),
                subsetCellByInfoUI(id, mini = TRUE),
                boxPlotControlUI(
                    id, withPoints = FALSE, withColor = TRUE,
                    withFontSize = FALSE)
            ),
            column(9, geneExprDotPlotUI(id))
        )
    )
}
plotWaffleServer <- function(id, dataSource, optCrt, postfix = 1) {
    moduleServer(id, function(input, output, session) {
        ## input column
        genelist <- dataSource()$sc1def$gene1
        if (!is.null(dataSource()$genelist)) {
            if (length(dataSource()$genelist) > 1) {
                genelist <- dataSource()$genelist[1]
            } else{
                genelist <- c(dataSource()$genelist, genelist)
            }
        }
        updateTextAreaInput(
            session, "genelist",
            value = paste0(genelist, collapse = ", "))
        updateSelectInput(
            session,
            "CellInfoX",
            "Group by:",
            choices = getGroupUI(dataSource),
            selected = dataSource()$sc1def$grp1
        )
        updateSelectInput(
            session,
            "CellInfoY",
            "Split by:",
            choices = getGroupUI(dataSource),
            selected = dataSource()$sc1def$grp2
        )
        updateSubsetCellUI(id, input, output, session, dataSource, addNA = TRUE)
        updateFilterCellUI(id, optCrt, input, output, session, dataSource)
        ### plots
        plotX <- reactive({
            scDRwafflePlot(
                dataSource()$dataset,
                dataSource()$sc1gene,
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$genelist,
                input$CellInfoX,
                input$CellInfoY,
                input$plotcols,
                input$subsetCell,
                getSubsetCellVal(input)
            )
        })
        updateGeneExprDotPlotUI(
            postfix,
            id,
            input,
            output,
            session,
            plotX,
            .globals$pList3[input$plotpsz],
            dataSource()$dataset,
            make.names(input$genelist)
        )
    })
}
