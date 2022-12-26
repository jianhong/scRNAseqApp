plotPieDimUI <- function(id){
  tabPanel(
    value = id,
    HTML("Sunburst"),
    h4("Gene expression scatter_pie plot"),
    "In this tab, users can visualise the gene expression patterns of ",
    "multiple genes grouped by categorical cell information",
    "(e.g. library / cluster).",
    "The expression of each gene will be re-scaled to 0-1.",
    "It will only plot top 5000 events.",
    br(),
    fluidRow(
      column(3, dimensionReductionUI(id)),
      column(3,
             subsetCellByInfoUI(id),
             subsetCellByFilterUI(id)),
      column(6, graphicsControlUI(id, GeneExpraspSelect='Fixed'))
    ),
    br(),
    fluidRow(
      column(
        3, style="border-right: 2px solid black",
        textAreaInput(NS(id, "genelist"),
                      HTML("List of gene names <br />
                            (Max 10 genes (over 5 will be busy), <br />
                             separated by , or ; or newline):"),
                      height = "200px",
                      value = NULL) %>%
          shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                              title = "List of genes to plot on Sunburst",
                              content = c("Input genes to plot",
                                          "- Maximum 10 genes (due to ploting space limitations)",
                                          "- Genes should be separated by comma, semicolon or newline")),
        checkboxInput(NS(id, "CoExpred"),
                      "Co-expressed (all>0)", value = TRUE),
        tagList(
          actionButton(NS(id, "CoExprtog"), "Toggle plot controls"),
          conditionalPanel(
            condition = paste0("input.CoExprtog", " % 2 == 1"), ns=NS(id),
            checkboxInput(NS(id, "CoExprCircle"),
                          "Circle Cells", value = FALSE),
            checkboxInput(NS(id, "CoExprBg"),
                          "Plot cells as Background", value = TRUE),
            checkboxInput(NS(id, "CoExprMarkGrp"), "lable groups",
                          value = FALSE),
            radioButtons(NS(id, "CoExprType"), "Plot type",
                         choices = c("sunburst", "pie", 'donut', "bar"),
                         selected = "sunburst"),
            sliderInput(NS(id, "CoExprAlpha"), "Transparency",
                        min = 0, max = 1, step = 0.01,
                        value = .8)
          )
        )
      ),
      column(
        9, h4(htmlOutput(NS(id, "oupTxt"))), geneExprDotPlotUI(id)
      )
    )
  )
}

plotPieDimServer <- function(id, dataSource, optCrt){
  moduleServer(id, function(input, output, session){
    ## input column 1
    ### Dimension Reduction
    updateDimRedSelInputPair(session, dataSource)
    ## input column 2
    updateSubsetCellUI(id, input, output, session, dataSource)
    updateFilterCellUI(id, optCrt, input, output, session, dataSource)
    ## input column
    genelist <- dataSource()$sc1def$genes
    if(!is.null(dataSource()$genelist)){
      if(length(dataSource()$genelist)>1){
        genelist <- dataSource()$genelist
      }else{
        genelist <- c(dataSource()$genelist, genelist)
      }
    }
    updateTextAreaInput(session, "genelist",
                        value = paste0(genelist[
                          seq.int(min(4, length(genelist)))],
                                       collapse = ", "))
    ### plots
    plot1 <- reactive({
      scPieDim(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        dataSource()$dataset,
        dataSource()$sc1gene,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$genelist,
        input$subsetCell,
        input$subsetCellVal,
        valueFilterKey=input$filterCell,
        valueFilterCutoff=input$filterCellVal,
        input$CoExpred,
        input$GeneExprsiz,
        input$CoExprCircle,
        input$CoExprBg,
        input$CoExprMarkGrp,
        input$CoExprAlpha,
        input$CoExprType,
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt)
    })
    updateGeneExprDotPlotUI(postfix=1, id, input, output, session,
                            plot1,
                            .globals$pList1[input$GeneExprpsz],
                            dataSource()$dataset,
                            input$GeneExprdrX,
                            input$GeneExprdrY,
                            'sunburst')
  })
}

