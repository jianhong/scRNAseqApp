#' @importFrom DT DTOutput
#' @importFrom magrittr %>%
coExpr3dUI <- function(id) {
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
                geneCoExprPlotControlUI(id, 1, plotly = TRUE)
            ),
            column(
                9,
                fluidRow(column(12, uiOutput(
                    NS0(id, "GeneExpr3Doup.ui", 1)
                ))),
                downloadButton(
                    NS(id, 'downloadExpr'),
                    "Download expression for clicked cell"
                ),
                verbatimTextOutput(NS(id, 'clicked'))
            )
        )
    )
}

#' @importFrom DT renderDT
#' @importFrom magrittr %>%
#' @importFrom plotly plotlyOutput renderPlotly event_data
coExpr3dServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## title
        output$GeneExpr <-
            renderUI({
                HTML(paste("3D Gene", dataSource()$terms['coexpression']))
            })
        ## subtitle
        output$GeneExprSubTitle <-
            renderUI({
                h4(paste(
                    sub(
                        substr(dataSource()$terms['coexpression'], 1, 1),
                        toupper(substr(
                            dataSource()$terms['coexpression'],
                            1, 1)),
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
        plot3d <- reactive({
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
                plotType="3D",
                pointSize=input$GeneExprsiz,
                GeneExprDotCol=input$CoExprcol1,
                GeneExprDotOrd=input$CoExprord1,
                labelsFontsize=input$GeneExprfsz,
                plotAspectRatio=input$GeneExprasp,
                keepXYlables=input$GeneExprtxt,
                hideFilterCell=input$CoExprhid1
            )
        })
        output$GeneExpr3Doup1 <- renderPlotly({
            plot3d()
        })
        output$GeneExpr3Doup.ui1 <- renderUI({
            plotlyOutput(
                NS0(id, "GeneExpr3Doup", 1),
                height = .globals$pList1[input$GeneExprpsz])
        })
        output$downloadExpr <- exprDownloadHandler(
            dataSource()$sc1gene,
            dataSource()$dataset,
            dataSource()$sc1meta)
        output$clicked <- plotly3d_click(session)
    })
}
