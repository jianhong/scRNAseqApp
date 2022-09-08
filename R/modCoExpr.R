coExprUI <- function(id){
  tabPanel(
    htmlOutput(NS(id, 'GeneExpr')),
    tabsubTitleUI(id, 'GeneExpr',
                  description = paste(
                    "In this tab, users can visualise the coexpression of two ",
                    "genes on low-dimensional representions."
                  )),
    fluidRow(
      column(3, dimensionReductionUI(id)),
      column(3, subsetCellByInfoUI(id)),
      column(6, graphicsControlUI(id))
    ),
    fluidRow(
      column(
        3, style="border-right: 2px solid black",
        htmlOutput(NS0(id, "subPlotTitle", 1)),
        geneExprUI(id, 1),
        geneExprUI(id, 2),
        geneCoExprPlotControlUI(id, 1)
      ),
      column(
        6, style="border-right: 2px solid black",
        geneExprDotPlotUI(id, 1)
      ),
      column(
        3,geneExprDotPlotUI(id, 2),
        br(), h4("Cell numbers"),
        dataTableOutput(NS(id, "coExpr.dt"))
      )
    )
  )
}
coExprServer <- function(id, dataSource, optCrt, currentdataset){
  moduleServer(id, function(input, output, session){
    ## title
    output$GeneExpr <-
      renderUI({HTML(paste("Gene", dataSource()$terms['coexpression']))})
    ## subtitle
    output$GeneExprSubTitle <-
      renderUI({
        h4(paste(sub(substr(dataSource()$terms['coexpression'], 1, 1),
                     toupper(substr(dataSource()$terms['coexpression'], 1, 1)),
                     dataSource()$terms['coexpression']),
                 "of two genes on reduced dimensions"))})
    ## input column 1
    ### Dimension Reduction
    updateSelectInput(session, "GeneExprdrX", "X-axis:",
                      choices = dataSource()$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource()$sc1def$dimred[1])
    updateSelectInput(session,"GeneExprdrY", "Y-axis:",
                      choices = dataSource()$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource()$sc1def$dimred[2])
    ## input column 2
    updateSelectInput(session,
                      "subsetCell",
                      "Cell information to subset:",
                      choices = dataSource()$sc1conf[grp == TRUE]$UI,
                      selected = dataSource()$sc1def$grp1)

    output$subsetCell.ui <- renderUI({
      if(input$subsetCell!=""){
        sub = strsplit(dataSource()$sc1conf[UI == input$subsetCell]$fID,
                       "\\|")[[1]]
        checkboxGroupInput(NS(id, "subsetCellVal"),
                           "Select which cells to show",
                           inline = TRUE,
                           choices = sub,
                           selected = sub)
      }
    })

    ## plot region
    ### sub region title
    output$subPlotTitle1 <-
      renderUI({h4(paste("Gene", dataSource()$terms['expression']))})
    ### dropdown list
    updateSelectizeInput(session, "GeneName1",
                         choices = sort(names(dataSource()$sc1gene)),
                         server = TRUE,
                         selected = dataSource()$sc1def$gene1,
                         options = list(maxOptions = 6, create = TRUE,
                                        persist = TRUE, render = I(optCrt)))
    updateSelectizeInput(session, "GeneName2",
                         choices = sort(names(dataSource()$sc1gene)),
                         server = TRUE,
                         selected = dataSource()$sc1def$gene2,
                         options = list(maxOptions = 6, create = TRUE,
                                        persist = TRUE, render = I(optCrt)))
    ### plots
    plot1 <- reactive({
      scDRcoex(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName1,
        input$GeneName2,
        input$subsetCell,
        input$subsetCellVal,
        dataSource()$dataset,
        "sc1gexpr.h5",
        dataSource()$sc1gene,
        input$GeneExprsiz,
        input$CoExprcol1,
        input$CoExprord1,
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt)
    })
    output$GeneExproup1 <- renderPlot({ plot1() })
    output$GeneExproup.ui1 <- renderUI({
      plotOutput(NS0(id, "GeneExproup", 1),
                 height = pList[input$GeneExprpsz])
    })
    output$GeneExproup.pdf1 <-
      plotsDownloadHandler(
        "pdf",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName1,
        input$GeneName2)
    output$GeneExproup.png1 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName1,
        input$GeneName2)

    plot2 <- reactive({
      scDRcoexLeg(
        input$GeneName1,
        input$GeneName2,
        input$CoExprcol1,
        input$GeneExprfsz)
    })
    output$GeneExproup2 <- renderPlot({ plot2() })
    output$GeneExproup.ui2 <- renderUI({
      plotOutput(NS0(id, "GeneExproup", 2),
                 height = 300)
    })
    output$GeneExproup.pdf2 <-
      plotsDownloadHandler(
        "pdf",
        width=input$GeneExproup.w2,
        height=input$GeneExproup.h2,
        plot2(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName1,
        input$GeneName2)
    output$GeneExproup.png2 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w2,
        height=input$GeneExproup.h2,
        plot2(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName1,
        input$GeneName2)

    output$coExpr.dt <- renderDataTable({
      ggData <- scDRcoexNum(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneName1,
        input$GeneName2,
        input$subsetCell,
        input$subsetCellVal,
        dataSource()$dataset,
        "sc1gexpr.h5",
        dataSource()$sc1gene)
      datatable(ggData, rownames = FALSE, extensions = "Buttons",
                options = list(pageLength = -1,
                               dom = "tB",
                               buttons = c("copy", "csv", "excel"))) %>%
        formatRound(columns = c("percent"), digits = 2)
    })
  })
}

