subsetGeneExprUI <- function(id) {
    tabPanel(
        value = id,
        htmlOutput(NS(id, 'GeneExpr')),
        tabsubTitleUI(
            id,
            'GeneExpr',
            description = paste(
                "In this tab, users can visualise gene expressions of",
                "two groups side-by-side on low-dimensional representions."
            )
        ),
        fluidRow(
            column(
                3,
                dimensionReductionUI(id),
                h4("Information to show"),
                selectInput(
                    NS(id, "GeneName"),
                    "Gene name:",
                    choices = NULL) %>% helper1(category = "geneName"),
                selectInput(
                    NS(id, "CellInfo"),
                    "Cell information to show:",
                    choices = NULL)
            ),
            column(
                4,
                subsetCellByInfoUI(id),
                subsetCellByFilterUI(id)),
            column(
                5,
                graphicsControlUI(id), br(),
                cellInfoTblUI(id, 1))
        ),
        fluidRow(
            column(
                6,
                style = "border-right: 2px solid black",
                htmlOutput(NS0(id, "subPlotTitle", 1)),
                fluidRow(column(6, uiOutput(
                    NS0(id, "GeneExprgrp.ui", 1)
                )),
                column(
                    6, geneExprPlotControlUI(id, postfix = 1)
                )),
                geneExprDotPlotUI(id, 1)
            ),
            column(
                6,
                htmlOutput(NS0(id, "subPlotTitle", 2)),
                fluidRow(column(6, uiOutput(
                    NS0(id, "GeneExprgrp.ui", 2)
                )),
                column(
                    6, geneExprPlotControlUI(id, postfix = 2)
                )),
                geneExprDotPlotUI(id, 2)
            )
        )
    )
}
subsetGeneExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## title
        output$GeneExpr <-
            renderUI({
                HTML(paste("Subset", dataSource()$terms["GeneExpr"]))
            })
        ## subtitle
        output$GeneExprSubTitle <-
            renderUI({
                h4(paste(
                    "Subset gene",
                    dataSource()$terms['expression'],
                    "on dimension reduction"
                ))
            })
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, dataSource)
        ### Information to show
        #### gene name to plot
        selectedGene <- dataSource()$sc1def$gene1
        if (!is.null(dataSource()$genelist)) {
            selectedGene <- dataSource()$genelist[1]
        }
        updateSelectizeInput(
            session,
            "GeneName",
            choices = sort(names(dataSource()$sc1gene)),
            server = TRUE,
            selected = selectedGene,
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        #### cell information to plot
        updateSelectInput(
            session,
            "CellInfo",
            "Cell information to show:",
            choices = dataSource()$sc1conf[dataSource()$sc1conf$grp == TRUE]$UI,
            selected = dataSource()$sc1def$grp1
        )
        
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource)
        updateFilterCellUI(id, optCrt, input, output, session, dataSource)
        
        ## plot region
        ### update the Color Range to make two plots comparable
        inpColRange <- reactive({
            scDRgene(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                gene1=input$GeneName,
                subsetCellKey=c(input$CellInfo, input$subsetCell),
                subsetCellVal=getSubsetCellVal(
                    input,
                    list(c(input$GeneExprsub1b, input$GeneExprsub2b)),
                    input$CellInfo),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                pointSize=input$GeneExprsiz,
                gradientCol=input$GeneExprcol1,
                GeneExprDotOrd=input$GeneExprord1,
                labelsFontsize=input$GeneExprfsz,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                inpPlt=input$GeneExprtype1,
                inpXlim=if (input$GeneExprxlimb1 %% 2 == 0)
                    0
                else
                    input$GeneExprxlim1,
                inpColRange = TRUE,
                valueFilterKey = input$filterCell,
                valueFilterCutoff = input$filterCellVal,
                hideFilterCell = input$GeneExprhid1
            )
        })
        ## plots
        getSubGroupName <- function(dataSource, input) {
            sub <- strsplit(
                dataSource()$sc1conf[
                    dataSource()$sc1conf$UI == input$CellInfo]$fID,
                "\\|")
            if (length(sub)) {
                sub <- sub[[1]]
            }
            sub
        }
        updateSubsetGeneExprPlot(
            1,
            getSubGroupName,
            optCrt,
            inpColRange,
            id,
            input,
            output,
            session,
            dataSource
        )
        updateSubsetGeneExprPlot(
            2,
            getSubGroupName,
            optCrt,
            inpColRange,
            id,
            input,
            output,
            session,
            dataSource
        )
        
        ## cell stats
        observeEvent(input$GeneExprsub1b, updateGeneExprDT())
        observeEvent(input$GeneExprsub2b, updateGeneExprDT())
        updateGeneExprDT <- function() {
            output$GeneExpr.dt1 <- renderDT({
                ggData <- scDRnum(
                    dataSource()$sc1conf,
                    dataSource()$sc1meta,
                    c(
                        input$CellInfo,
                        input$GeneExprsub1b,
                        input$GeneExprsub2b
                    ),
                    input$GeneName,
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
        }
    })
}
