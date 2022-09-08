cellInfoCellInfoUI <- function(id){
  tabPanel(
    HTML("CellInfo vs CellInfo"),
    h4("Cell information vs cell information on dimension reduction"),
    "In this tab, users can visualise two cell informations side-by-side ",
    "on low-dimensional representions.",
    br(),br(),
    fluidRow(
      column(3, dimensionReductionUI(id)),
      column(3, subsetCellByInfoUI(id)),
      column(6, graphicsControlUI(id))
    ),
    fluidRow(
      column(
        6, style="border-right: 2px solid black", h4("Cell information 1"),
        fluidRow(
          column(6, cellInfoUI(id, 1)),
          column(6, cellInfoPlotControlUI(id, 1))
        ),
        geneExprDotPlotUI(id, 1)
      ),
      column(
        6, h4("Cell information 2"),
        fluidRow(
          column(6, cellInfoUI(id, 2)),
          column(6, cellInfoPlotControlUI(id, 2))
        ),
        geneExprDotPlotUI(id, 2)
      )
    )
  )
}
cellInfoCellInfoServer <- function(id, dataSource, optCrt, currentdataset){
  moduleServer(id, function(input, output, session){
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
    ### dropdown list
    updateSelectInput(session, "CellInfo1", "Cell information:",
                      choices = dataSource()$sc1conf$UI,
                      selected = dataSource()$sc1def$meta1)
    updateSelectInput(session, "CellInfo2", "Cell information:",
                      choices = dataSource()$sc1conf$UI,
                      selected = dataSource()$sc1def$meta2)
    ### plots
    plot1 <- reactive({
      scDRcell(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$CellInfo1,
        input$subsetCell,
        input$subsetCellVal,
        input$GeneExprsiz,
        input$CellInfocol1,
        input$CellInfoord1,
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input$CellInfolab1)
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
        input$CellInfo1)
    output$GeneExproup.png1 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$CellInfo1)

    plot2 <- reactive({
      scDRcell(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$CellInfo2,
        input$subsetCell,
        input$subsetCellVal,
        input$GeneExprsiz,
        input$CellInfocol2,
        input$CellInfoord2,
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input$CellInfolab2)
    })
    output$GeneExproup2 <- renderPlot({ plot2() })
    output$GeneExproup.ui2 <- renderUI({
      plotOutput(NS0(id, "GeneExproup", 2),
                 height = pList[input$GeneExprpsz])
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
        input$CellInfo2)
    output$GeneExproup.png2 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w2,
        height=input$GeneExproup.h2,
        plot2(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$CellInfo2)
  })
}

