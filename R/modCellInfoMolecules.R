cellInfoMolUI <- function(id) {
    tabPanel(
        value = id,
        HTML("CellInfo vs Molecules"),
        tagList(
            h4('Cell information vs Molecules'),
            paste(
                "In this tab, users can visualise both cell and molecule",
                "information side-by-side on low-dimensional representions."
            ),
            br(),br()
        ),
        fluidRow(
            column(3, dimensionReductionUI(id)),
            column(5, subsetCellByInfoUI(id)),
            column(4, graphicsControlUI(id))
        ),
        fluidRow(
            resizablePlotContainer(
                leftUI=tagList(
                    h4("Cell information"),
                    fluidRow(
                        column(6, cellInfoUI(id, 1)),
                        column(6, cellInfoPlotControlUI(id, 1, linkXYlim = TRUE))),
                    geneExprDotPlotUI(id, 1, editor=TRUE)
                ),
                rightUI=tagList(
                    h4("Molecules"),
                    fluidRow(
                        column(5, 
                               selectInput(
                                   NS0(id, "GeneName", 2),
                                   'Molecule name:', choices=NULL,
                                   multiple = TRUE)),
                        column(2, 
                               selectInput(
                                   NS0(id, "fov", 2), 
                                   'FOV:', choices=NULL)),
                        column(5, 
                               actionButton(
                                   NS0(id, "GeneExprtog", 2),
                                   "Toggle plot controls"),
                               conditionalPanel(
                                   condition = paste0("input.GeneExprtog", 2, " % 2 == 1"),
                                   ns=NS(id),
                                   radioButtons(
                                       NS0(id, "GeneExprcol", 2), "Colour:",
                                       inline = TRUE,
                                       choices = rownames(brewer.pal.info),
                                       selected = 'Set1'),
                                   actionButton(
                                       NS0(id, "manuXYlimTog", 2),
                                       "Manually set x/y axis", inline = TRUE),
                                   conditionalPanel(
                                       condition = paste0(
                                           "input.manuXYlimTog", 2, " % 2 ==1"),
                                       ns=NS(id),
                                       sliderInput(
                                           NS0(id, "manuXlim", 2), "Xlim range:",
                                           min = -10, max = 100,
                                           value = c(-1.5, 10),
                                           step = 0.1),
                                       sliderInput(
                                           NS0(id, "manuYlim", 2), "Ylim range:",
                                           min = -10, max = 100,
                                           value = c(-1.5, 10),
                                           step = 0.1),
                                       manuXYlimOriUI(id, 2),
                                       conditionalPanel(
                                           condition = paste0("input.manuXYlimTog",
                                                              1,
                                                              " % 2 ==1"),
                                           ns=NS(id),
                                           checkboxInput(
                                               NS0(id, 'XYlimLinker', 2),
                                               "Link X/Y Axis",
                                               value = TRUE) ## must keep same as paired XYlimLinker
                                       )
                                   )
                                )
                            )
                        ),
                    geneExprDotPlotUI(id, 2),
                    HTML('Note: subset cells will not work for molecules.')
                )
            )
        )
    )
}
#' @importFrom DT formatRound renderDT
#' @importFrom magrittr %>%
cellInfoMolServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, input, dataSource)
        ## update XYlimLinker with Reduction
        observeEvent(input[['GeneExprdrX']], {
            rdim <- sub('.$', '', input[['GeneExprdrX']])
            updateCheckboxInput(
                inputId = 'XYlimLinker2',
                value = isTRUE(rdim==input[['fov2']])
            )
            updateCheckboxInput(
                inputId = 'XYlimLinker1',
                value = isTRUE(rdim==input[['fov2']])
            )
        })
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource)
        
        ## plot region
        ### cellInfo
        updateCellInfoPlot(1, id, input, output, session, dataSource)
        ### expression stats table
        output$GeneExpr.dt1 <- renderDT({
            ggData <- scDRnum(
                inpConf=dataSource()$sc1conf,
                inpMeta=dataSource()$sc1meta,
                inpCellInfo=input$CellInfo1,
                gene=input$GeneName2,
                inpsubName=input$subsetCell,
                inpsubValue=getSubsetCellVal(input),
                dataset=dataSource()$dataset,
                geneIdMap=dataSource()$sc1gene,
                inpsplt=input$GeneExprsplt1
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
        
        ### molecule
        selectedGene <- dataSource()$sc1def$gene1
        if (!is.null(dataSource()$genelist)) {
            selectedGene <- dataSource()$genelist[1]
        }
        updateMoleculePlot(
            postfix = 2,
            selectedGene,
            optCrt,
            id,
            input,
            output,
            session,
            dataSource)
    })
}
