plotVioBoxUI <- function(id) {
    tabPanel(
        value = id,
        HTML("Violinplot / Boxplot"),
        h4("Cell information / gene expression violin plot / box plot"),
        "In this tab, users can visualise the gene expression ",
        "or continuous cell information ",
        "(e.g. Number of UMIs / module score) across groups of cells ",
        "(e.g. libary / clusters).",
        br(),
        br(),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                xaxisCellInfoUI(id),
                subsetCellByInfoUI(id, mini = TRUE),
                subsetCellByFilterUI(
                    id,
                    label = "Cell Info / Gene name (Y-axis):",
                    title = "Cell Info / Gene to plot",
                    content = c(
                        "Select cell info / gene to plot on Y-axis",
                        "- Can be continuous cell information ",
                        "(e.g. nUMIs / scores)",
                        "- Can also be gene expression"
                    )
                ),
                radioButtons(
                    NS(id, "plottyp"),
                    "Plot type:",
                    choices = c("violin", "boxplot"),
                    selected = "violin",
                    inline = TRUE
                ),
                checkboxInput(
                    NS(id, "plotpts"), "Show data points",
                    value = FALSE),
                checkboxInput(
                    NS(id, "plotord"),
                    "Reorder the contents", value = FALSE
                ),
                conditionalPanel(
                    condition = "input.plotord % 2 == 1",
                    ns=NS(id),
                    uiOutput(outputId = NS(id, "plotXord"))
                ),
                boxPlotControlUI(id)
            ),
            column(9, geneExprDotPlotUI(id))
        )
    )
}
plotVioBoxServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## input column
        updateSelectInput(
            session,
            "CellInfoX",
            "Cell information (X-axis):",
            choices = getGroupUI(dataSource),
            selected = dataSource()$sc1def$grp1
        )
        
        updateSubsetCellUI(id, input, output, session, dataSource, addNA = TRUE)
        updateFilterCellUI(id, optCrt, input, output, session, dataSource)
        updateRankList(
            input, output, dataSource, "CellInfoX", "plotXord",
            NS(id, "cellinfoXorder"))
        
        ## plot region
        ### plots
        plot1 <- reactive({
            scVioBox(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$CellInfoX,
                input$filterCell,
                input$subsetCell,
                getSubsetCellVal(input),
                input$filterCell,
                input$filterCellVal,
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$plottyp,
                input$plotpts,
                input$plotsiz,
                input$plotfsz,
                reorder = input$plotord,
                orderX = input$cellinfoXorder
            )
        })
        updateGeneExprDotPlotUI(
            postfix = 1,
            id,
            input,
            output,
            session,
            plot1,
            .globals$pList2[input$plotpsz],
            dataSource()$dataset,
            input$plottyp,
            input$CellInfoX,
            input$filterCell
        )
        
    })
}
