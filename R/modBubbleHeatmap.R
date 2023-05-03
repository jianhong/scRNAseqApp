plotBubbleHeatmapUI <- function(id) {
    tabPanel(
        value = id,
        HTML("Bubbleplot / Heatmap"),
        h4("Gene expression bubbleplot / heatmap"),
        "In this tab, users can visualise the gene expression patterns of ",
        "multiple genes grouped by categorical cell information",
        "(e.g. library / cluster).",
        br(),
        "The normalised expression are averaged,",
        "log-transformed and then plotted.",
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
                            (Max 500 genes (over 50 will response slow), <br />
                            separated by , or ; or newline):"
                    ),
                    height = "200px",
                    value = NULL
                ) %>%
                    shinyhelper::helper(
                        type = "inline",
                        size = "m",
                        fade = TRUE,
                        title = "List of genes to plot on bubbleplot / heatmap",
                        content = c(
                            "Input genes to plot",
                            "- Maximum 500 genes
                            (due to ploting space limitations)",
                            "- Genes should be separated by comma,
                            semicolon or newline"
                        )
                    ),
                xaxisCellInfoUI(id),
                subsetCellByInfoUI(id, mini = TRUE),
                sliderInput(
                    NS(id, "filterVal"),
                    "Filter the cells by value",
                    min = 0,
                    max = 10,
                    value = 0
                ),
                radioButtons(
                    NS(id, "plottyp"),
                    "Plot type:",
                    choices = c("Bubbleplot", "Heatmap", "Violin"),
                    selected = "Bubbleplot",
                    inline = TRUE
                ),
                checkboxInput(
                    NS(id, "plotscl"),
                    "Scale gene expression",
                    value = TRUE),
                checkboxInput(
                    NS(id, "plotrow"),
                    "Cluster rows (genes)",
                    value = TRUE),
                checkboxInput(
                    NS(id, "plotcol"),
                    "Cluster columns (samples)",
                    value = FALSE),
                checkboxInput(
                    NS(id, "plotflp"),
                    "Flip X/Y", value = FALSE),
                conditionalPanel(
                    condition = paste0("input.plottyp == 'Heatmap'"),
                    ns = NS(id),
                    checkboxInput(
                        NS(id, "plotall"),
                        "All cells (time consuming)",
                        value = FALSE)
                ),
                conditionalPanel(
                    condition = "input.plottyp != 'Violin'",
                    ns = NS(id),
                    actionButton(
                        NS(id, "userbreaks"),
                        "Set ColorKey range"),
                    conditionalPanel(
                        condition = "input.userbreaks % 2 ==1",
                        ns = NS(id),
                        numericInput(NS(id, "colorb1"), "min cutoff", value = -10),
                        numericInput(NS(id, "colorb2"), "max cutoff", value = 10),
                        actionButton(NS(id, "setcolrg"), "Apply colorkey range")
                    )
                ),
                conditionalPanel(
                    condition = "input.plottyp == 'Violin'",
                    ns = NS(id),
                    checkboxInput(
                        NS(id, "plotpts"), "Show data points",
                        value = FALSE),
                    sliderInput(
                        NS(id, "plotsiz"), "Data point size:",
                        min = 0, max = 4, value = 1.25, step = 0.25)
                ),
                conditionalPanel(
                    condition = "input.plotcol % 2 != 1",
                    ns = NS(id),
                    checkboxInput(
                        NS(id, "plotord"),
                        "Reorder the contents", value = FALSE
                    ),
                    conditionalPanel(
                        condition = "input.plotord % 2 == 1",
                        ns=NS(id),
                        uiOutput(outputId = NS(id, "plotXord"))
                    )
                ),
                br(),
                boxPlotControlUI(id, withPoints = FALSE, withColor = TRUE)
            ),
            column(9, h4(htmlOutput(
                NS(id, "oupTxt")
            )), geneExprDotPlotUI(id))
        )
    )
}

plotBubbleHeatmapServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        colrg <- reactiveVal(NA)
        ## input column
        genelist <- dataSource()$sc1def$genes
        if (!is.null(dataSource()$genelist)) {
            if (length(dataSource()$genelist) > 1) {
                genelist <- dataSource()$genelist
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
        updateSubsetCellUI(id, input, output, session, dataSource, addNA = TRUE)
        
        observeEvent(input$userbreaks, {
            rg <- scBubbHeat(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$genelist,
                input$CellInfoX,
                input$subsetCell,
                input$subsetCellVal,
                input$filterVal,
                input$plottyp,
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$plotscl,
                input$plotrow,
                input$plotcol,
                input$plotcols,
                input$plotpts,
                input$plotfsz,
                input$plotflp,
                input$plotall,
                legendTitle = dataSource()$terms['expression'],
                returnColorRange = TRUE
            )
            rg <- round(rg, digits = 2)
            updateNumericInput(
                session,
                "colorb1",
                value = rg[2],
                min = floor(rg[1]),
                max = ceiling(rg[4]),
                step = .01
            )
            updateNumericInput(
                session,
                "colorb2",
                value = rg[3],
                min = ceiling(rg[1]),
                max = ceiling(rg[4]),
                step = .01
            )
            
        })
        observeEvent(input$setcolrg, {
            colrg(sort(c(
                input$colorb1, input$colorb2
            )))
        })
        
        updateRankList(
            input, output, dataSource, "CellInfoX", "plotXord",
            NS(id, "cellinfoXorder"))
        
        ## update the ui
        output$oupTxt <- renderUI({
            geneList <- scGeneList(input$genelist, dataSource()$sc1gene)
            if (nrow(geneList) > 50) {
                HTML("More than 50 input genes! Please reduce the gene list!")
            } else {
                oup <- paste0(
                    nrow(geneList[geneList$present == TRUE]),
                    " genes OK and will be plotted")
                if (nrow(geneList[geneList$present == FALSE]) > 0) {
                    oup <- paste0(
                        oup,
                        "<br/>",
                        nrow(geneList[geneList$present == FALSE]),
                        " genes not found (",
                        paste0(
                            geneList[geneList$present == FALSE]$gene,
                            collapse = ", "),
                        ")"
                    )
                }
                HTML(oup)
            }
        })
        
        ## plot region
        ### plots
        plot1 <- reactive({
            ht <- scBubbHeat(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$genelist,
                input$CellInfoX,
                input$subsetCell,
                input$subsetCellVal,
                input$filterVal,
                input$plottyp,
                dataSource()$dataset,
                dataSource()$sc1gene,
                input$plotscl,
                input$plotrow,
                input$plotcol,
                input$plotcols,
                if(input$plotpts %% 2 ==0) 0 else input$plotsiz,
                input$plotfsz,
                input$plotflp,
                input$plotall,
                colorBreaks = 
                    if (input$userbreaks %% 2 == 0 && !is.na(colrg()[1]))
                        NA
                    else
                        colrg(),
                legendTitle = dataSource()$terms['expression'],
                reorder=input$plotord,
                orderX = input$cellinfoXorder
            )
        })
        observeEvent(
            input$plottyp,
            updateGeneExprDotPlotUI(
                postfix = 1,
                id,
                input,
                output,
                session,
                plot1,
                .globals$pList3[input$plotpsz],
                dataSource()$dataset,
                input$plottyp,
                input$CellInfoX,
                handlerFUN = if (input$plottyp == "Heatmap") {
                    heatmapDownloadHandler
                } else
                    plotsDownloadHandler
            )
        )
    })
}
