plotPieDimUI <- function(id, type='expr') {
    stopifnot(type %in% c('expr', 'cellinfo'))
    tabPanel(
        value = id,
        HTML(ifelse(type=='expr', "Sunburst", "Cellinfo deconvolution")),
        h4(ifelse(type=='expr', "Gene expression scatter_pie plot", 
                  "Cell information deconvolution plot")),
        "In this tab, users can visualise the ",
        ifelse(type=='expr', "gene expression", "cell information"),
        " patterns of multiple ",
        ifelse(type=='expr', "genes", "cell information"),
        if(type=='expr'){
            paste(" grouped by categorical cell information ",
            "(e.g. library / cluster). ",
            "The expression of each gene will be re-scaled to 0-1. ")
        }else{
            '. It is best choice to explor the cell deconvolution information.'
        },
        "It will only plot top 5000 events.",
        br(),br(),
        fluidRow(
            column(3, dimensionReductionUI(id)),
            column(
                5,
                subsetCellByInfoUI(id),
                subsetCellByFilterUI(id)),
            column(4, graphicsControlUI(id, GeneExpraspSelect = 'Fixed'))
        ),
        br(),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                div(style='display:none', 
                    textInput(inputId=NS(id, 'sunburst_type'),
                              label='', value=type)),
                if(type=='expr'){
                    tagList(
                        textAreaInput(
                            NS(id, "genelist"),
                            HTML(
                                "List of gene names <br />
                            (Max 10 genes (over 5 will be busy), <br />
                            separated by , or ; or newline):"
                            ),
                            height = "200px",
                            value = NULL
                        ) %>%
                            shinyhelper::helper(
                                type = "inline",
                                size = "m",
                                fade = TRUE,
                                title = "List of genes to plot on Sunburst",
                                content = c(
                                    "Input genes to plot",
                                    "- Maximum 10 genes",
                                    "(due to plot space limitations)",
                                    "- Genes should be separated by comma,",
                                    "semicolon or newline"
                                )
                            ),
                        checkboxInput(
                            NS(id, "CoExpred"),
                            "Co-expressed (all>0)", value = TRUE)
                    )
                }else{
                    selectInput(
                        NS(id, "genelist"), 
                        label = "List of cell informations",
                        choices = c(), selected = c(),
                        multiple = TRUE)
                },
                tagList(
                    actionButton(NS(id, "CoExprtog"), "Toggle plot controls"),
                    conditionalPanel(
                        condition = paste0("input.CoExprtog", " % 2 == 1"),
                        ns = NS(id),
                        checkboxInput(
                            NS(id, "CoExprCircle"),
                            "Circle Cells", value = FALSE),
                        checkboxInput(
                            NS(id, "CoExprBg"),
                            "Plot cells as Background", value = TRUE),
                        checkboxInput(
                            NS(id, "CoExprMarkGrp"), "lable groups",
                            value = FALSE),
                        radioButtons(
                            NS(id, "CoExprType"),
                            "Plot type",
                            choices = if(type=='expr'){
                                    c("sunburst", "pie", 'donut', "bar", "density",
                                      "sum", "max", "mean")
                                }else{
                                    c("sunburst", "pie", 'donut', "bar", "density")
                                },
                            selected = "sunburst"
                        ),
                        conditionalPanel(
                            condition = "['sum', 'max', 'mean'].includes(input.CoExprType)",
                            ns=NS(id),
                            radioButtons(
                                NS(id, "CoExprCol"), "Colour:",
                                inline = TRUE,
                                choices = availableThemes("sequence"),
                                selected = availableThemes("sequence")[1])),
                        sliderInput(
                            NS(id, "CoExprAlpha"),
                            "Transparency",
                            min = 0,
                            max = 1,
                            step = 0.01,
                            value = .8
                        )
                    )
                )
            ),
            column(9, h4(htmlOutput(
                NS(id, "oupTxt")
            )), geneExprDotPlotUI(id))
        )
    )
}

plotPieDimServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## input column 1
        ### Dimension Reduction
        updateDimRedSelInputPair(session, input, dataSource)
        ## input column 2
        updateSubsetCellUI(id, input, output, session, dataSource)
        updateFilterCellUI(id, optCrt, input, output, session, dataSource)
        ## input column
        if(input$sunburst_type=='expr'){
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
                value = paste0(
                    genelist[seq.int(min(4, length(genelist)))],
                    collapse = ", "))
        }else{
            choices <- dataSource()$sc1conf[!dataSource()$sc1conf$grp]$UI
            updateSelectizeInput(
                session, "genelist",
                choices = choices,
                selected = c()
            )
        }
        
        ### plots
        plot1 <- reactive({
            scPieDim(
                inpConf = dataSource()$sc1conf,
                inpMeta = dataSource()$sc1meta,
                dataset = dataSource()$dataset,
                geneIdMap = dataSource()$sc1gene,
                dimRedX = input$GeneExprdrX,
                dimRedY = input$GeneExprdrY,
                genelist = input$genelist,
                subsetCellKey = input$subsetCell,
                subsetCellVal = getSubsetCellVal(input),
                valueFilterKey = input$filterCell,
                valueFilterCutoff = input$filterCellVal,
                valueFilterCutoff2 = input$filterCellVal2,
                CoExpred = ifelse(input$sunburst_type=='expr',
                                  input$CoExpred, FALSE),
                pointSize = input$GeneExprsiz,
                lableCircle = input$CoExprCircle,
                plotCellBg = input$CoExprBg,
                markGrp = input$CoExprMarkGrp,
                alpha = input$CoExprAlpha,
                gradientCol= input$CoExprCol,
                plotType = input$CoExprType,
                labelsFontsize = input$GeneExprfsz,
                labelsFontFamily=input$GeneExprfml,
                plotAspectRatio = input$GeneExprasp,
                keepXYlables = input$GeneExprtxt,
                sunburst_type = input$sunburst_type
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
            'sunburst'
        )
    })
}
