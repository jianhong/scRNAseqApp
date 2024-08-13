#' @importFrom DT DTOutput
#' @importFrom magrittr %>%
coExprUI <- function(id) {
    tabPanel(
        value = id,
        htmlOutput(NS(id, 'GeneExpr')),
        tabsubTitleUI(
            id,
            'GeneExpr',
            description = paste(
                "In this tab, users can visualise the coexpression of two ",
                "genes on low-dimensional representions."
            )
        ),
        fluidRow(
            column(3, dimensionReductionUI(id)),
            column(5, subsetCellByInfoUI(id)),
            column(4, graphicsControlUI(id))
        ),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                htmlOutput(NS0(id, "subPlotTitle", 1)),
                geneExprUI(id, 1),
                geneExprUI(id, 2),
                geneCoExprPlotControlUI(id, 1)
            ),
            column(
                9,
                fluidRow(
                    column(
                        8, style = "border-right: 2px solid black",
                        geneExprDotPlotUI(id, 1)),
                    column(
                        4,
                        geneExprDotPlotUI(id, 2),
                        br(),
                        h4("Cell numbers"),
                        DTOutput(NS(id, "coExpr.dt"))
                    )
                ))
        )
    )
}
#' @importFrom DT renderDT
#' @importFrom magrittr %>%
#' @importFrom plotly plotlyOutput renderPlotly
coExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## title
        output$GeneExpr <-
            renderUI({
                HTML(paste("Gene", dataSource()$terms['coexpression']))
            })
        ## subtitle
        output$GeneExprSubTitle <-
            renderUI({
                h4(paste(
                    sub(
                        substr(dataSource()$terms['coexpression'], 1, 1),
                        toupper(substr(
                            dataSource()$terms['coexpression'], 1, 1)),
                        dataSource()$terms['coexpression']
                    ),
                    "of two genes on reduced dimensions"
                ))
            })
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, dataSource)
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource)
        
        ## plot region
        ### sub region title
        output$subPlotTitle1 <-
            renderUI({
                h4(paste("Gene", dataSource()$terms['expression']))
            })
        ### dropdown list
        updateSelectizeInput(
            session,
            "GeneName1",
            choices = sort(names(dataSource()$sc1gene)),
            server = TRUE,
            selected = dataSource()$sc1def$gene1,
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
            selected = dataSource()$sc1def$gene2,
            options = list(
                maxOptions = .globals$maxNumGene,
                create = TRUE,
                persist = TRUE,
                render = I(optCrt)
            )
        )
        ### plots
        plot1 <- reactive({
            scDRcoex(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                dimRedX=input$GeneExprdrX,
                dimRedY=input$GeneExprdrY,
                gene1=input$GeneName1,
                gene2=input$GeneName2,
                subsetCellKey=input$subsetCell,
                subsetCellVal=getSubsetCellVal(input),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                plotType="2D",
                pointSize=input$GeneExprsiz,
                GeneExprDotCol=input$CoExprcol1,
                GeneExprDotOrd=input$CoExprord1,
                labelsFontsize=input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                hideFilterCell=input$CoExprhid1
            )
        })
        updateGeneExprDotPlotUI(
            postfix = 1,
            id,
            input,
            output,
            session,
            plot1,
            .globals$pList1[input$GeneExprpsz],
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input$GeneName1,
            input$GeneName2
        )
        
        plot2 <- reactive({
            scDRcoexLeg(input$GeneName1,
                        input$GeneName2,
                        colorPairs = input$CoExprcol1,
                        labelsFontsize = input$GeneExprfsz,
                        labelsFontFamily=input$GeneExprfml)
        })
        updateGeneExprDotPlotUI(
            postfix = 2,
            id,
            input,
            output,
            session,
            plot2,
            300,
            dataSource()$dataset,
            input$GeneExprdrX,
            input$GeneExprdrY,
            input$GeneName1,
            input$GeneName2
        )
        
        output$coExpr.dt <- renderDT({
            ggData <- scDRcoexNum(
                dataSource()$sc1conf,
                dataSource()$sc1meta,
                input$GeneName1,
                input$GeneName2,
                input$subsetCell,
                getSubsetCellVal(input),
                dataSource()$dataset,
                dataSource()$sc1gene
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
                formatRound(columns = c("percent"),
                            digits = 2)
        })
    })
}
