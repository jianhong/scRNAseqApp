#' @importFrom DT DTOutput
#' @importFrom magrittr %>%
coMultiExprUI <- function(id) {
    tabPanel(
        value = id,
        htmlOutput(NS(id, 'GeneExpr')),
        tabsubTitleUI(
            id,
            'GeneExpr',
            description = paste(
                "In this tab, users can visualise the ",
                "Reciprocal Expression Index (REI) of coexpression of multiple ",
                "gene pairs on bar plots."
            )
        ),
        fluidRow(
            column(
                3,
                style = "border-right: 2px solid black",
                htmlOutput(NS0(id, "subPlotTitle", 1)),
                textAreaInput(
                    NS(id, "genelist"),
                    HTML(paste(
                        "List of gene pairs <br /> (Max", 
                        .globals$maxNumGenePair,
                        "genes, <br />separated by , and end by ; or newline for each pair):"
                    )),
                    height = "200px",
                    value = NULL
                ) %>%
                    shinyhelper::helper(
                        type = "inline",
                        size = "m",
                        fade = TRUE,
                        title = "List of gene pairs",
                        content = c(
                            "Input genes to plot",
                            paste0("- Maximum ", .globals$maxNumGenePair, " genes
                            (due to plot space limitations)"),
                            "- Gene pairs should be separated by comma,
                            and end by semicolon or newline"
                        )
                    ),
                numericInput(NS(id, 'K'),
                             label = "Pseudo-count",
                             value = 3) %>%
                    shinyhelper::helper(
                        type = "inline",
                        size = "m",
                        fade = TRUE,
                        title = "Pseudo-count",
                        content = c(
                            "The Reciprocal Expression Index an adjusted index of reciprocal expression by the formula:",
                            "$$ 2 \\cdot \\left( \\frac{\\max(x) + K}{\\sum(x) + 2K} - 0.5 \\right) $$",
                            ", where x is the count number of cells with expression of each gene in the pair.",
                            "and K is used to smooth the stochastic distribution of low counts. A higher K pulls the index closer to 0"
                        )
                    ),
                xaxisCellInfoUI(id),
                subsetCellByInfoUI(id, mini = TRUE),
                br(),
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
                conditionalPanel(
                    condition = "input.CellInfoX != ''",
                    ns = NS(id),
                    checkboxInput(
                        NS(id, "plotgord"),
                        "Reorder the groups", value = FALSE
                    ),
                    conditionalPanel(
                        condition = "input.plotgord % 2 == 1",
                        ns=NS(id),
                        uiOutput(outputId = NS(id, "plotGord"))
                    )
                ),
                br(),
                boxPlotControlUI(
                    id,
                    withPoints = FALSE,
                    withColor = FALSE,
                    withFontSize = TRUE)
            ),
            column(
                9,
                geneExprDotPlotUI(id, 1),
                br(),
                h4("Cell numbers"),
                DTOutput(NS(id, "coExpr.dt"))
            )
        )
    )
}
#' @importFrom DT renderDT
#' @importFrom magrittr %>%
#' @importFrom plotly plotlyOutput renderPlotly
coMultiExprServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        ## title
        output$GeneExpr <-
            renderUI({
                HTML(paste("Multipair", dataSource()$terms['coexpression']))
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
                    "of gene pairs on reduced dimensions"
                ))
            })
        ## input column
        genelist <- dataSource()$sc1def$genes
        if (!is.null(dataSource()$genelist)) {
            if (length(dataSource()$genelist) > 1) {
                genelist <- dataSource()$genelist
            } else{
                genelist <- c(dataSource()$genelist, genelist)
            }
        }
        
        updateSubsetCellUI(id, input, output, session, dataSource, addNA = TRUE)
        updateFilterCellUI(id, optCrt, input, output, session, dataSource)
        
        observeEvent(input$CellInfoX,
                     updateCheckboxInput(session, 'plotord', value = FALSE))
        updateRankList(
            input, output, dataSource, "CellInfoX", "plotGord",
            NS(id, "cellinfoGorder"))
        observeEvent(input$genelist, {
            geneList <- scGenePairs(input$genelist, dataSource()$sc1gene)
            geneList <- geneList[, list(gene = paste(.SD$gene, collapse = ', ')),
                                 by="pairId"]
            output[["plotXord"]] <- renderUI(rank_list(
                text = "Drag, drop and re-order the following items:",
                labels = sort(as.character(
                    geneList$gene
                )),
                input_id = NS(id, "cellinfoXorder")
            ))
        })
        ## fix the issue if the genelist is a string
        genelist <- paste(genelist, collapse=',')
        if(length(genelist)==1){
            genelist <- scGeneList(genelist, dataSource()$sc1gene)
            genelist <- genelist$gene[genelist$present]
        }
        ## default value must be an even number
        genelist <- genelist[seq.int(2*floor(length(genelist)/2))]
        updateTextAreaInput(
            session, "genelist",
            value = paste(apply(matrix(genelist, ncol=2), 1, paste, collapse = ", "), collapse=';\n'))
        updateSelectInput(
            session,
            "CellInfoX",
            "Group by:",
            choices = c(NA, getGroupUI(dataSource)),
            selected = NA
        )
        
        ## plot region
        ### sub region title
        output$subPlotTitle1 <-
            renderUI({
                h4(paste("Gene", dataSource()$terms['expression']))
            })
        ### plots
        plot1 <- reactive({
            scMultiCoex(
            inpConf = dataSource()$sc1conf,
            inpMeta = dataSource()$sc1meta,
            inp = input$genelist,
            inpGrp = input$CellInfoX,
            grpKey = input$subsetCell,
            grpVal = getSubsetCellVal(input),
            inpGrp1c = input$filterVal,
            inpPlt = input$plottyp,
            dataset = dataSource()$dataset,
            geneIdMap = dataSource()$sc1gene,
            inpScl = input$plotscl,
            inpRow = input$plotrow,
            inpCol = input$plotcol,
            inpcols = input$plotcols,
            pointSize = input$plotpts,
            labelsFontsize = input$plotfsz,
            labelsFontFamily = input$plotfml,
            reorder = input$plotord,
            orderX = input$cellinfoXorder,
            greorder = input$plotgord,
            orderG = input$cellinfoGorder,
            K = input$K #TODO, need a better name
            )
        })
        
        updateGeneExprDotPlotUI(
            postfix = 1,
            id = id,
            input = input,
            output = output,
            session = session,
            plotX = plot1,
            height = .globals$pList1[input$plotpsz],
            dataSource()$dataset,
            input$CellInfoX,
            input$CellInfoY,
            dataSource = dataSource
        )
        
        output$coExpr.dt <- renderDT({
            ggData <- 
                scMultiCoex(
                    inpConf = dataSource()$sc1conf,
                    inpMeta = dataSource()$sc1meta,
                    inp = input$genelist,
                    inpGrp = input$CellInfoX,
                    grpKey = input$subsetCell,
                    grpVal = getSubsetCellVal(input),
                    inpGrp1c = input$filterVal,
                    inpPlt = input$plottyp,
                    dataset = dataSource()$dataset,
                    geneIdMap = dataSource()$sc1gene,
                    inpScl = input$plotscl,
                    inpRow = input$plotrow,
                    inpCol = input$plotcol,
                    inpcols = input$plotcols,
                    pointSize = input$plotpts,
                    labelsFontsize = input$plotfsz,
                    labelsFontFamily = input$plotfml,
                    reorder = input$plotord,
                    orderX = input$cellinfoXorder,
                    greorder = input$plotgord,
                    orderG = input$cellinfoGorder,
                    K = input$K,
                    countOnly = TRUE
                )
            
            datatable(
                ggData,
                rownames = FALSE,
                extensions = "Buttons",
                options = list(
                    pageLength = -1,
                    dom = "tB",
                    buttons = c("copy", "csv", "excel"),
                    order = list(
                        list(0, 'asc'),
                        list(1, 'asc')
                    )
                )
            ) %>%
                formatRound(columns = c("REI"),
                            digits = 2)
        })
    })
}
