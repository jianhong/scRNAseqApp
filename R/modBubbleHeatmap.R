plotBubbleHeatmapUI <- function(id){
  tabPanel(
    HTML("Bubbleplot / Heatmap"),
    h4("Gene expression bubbleplot / heatmap"),
    "In this tab, users can visualise the gene expression patterns of ",
    "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(),
    "The normalised expression are averaged, log-transformed and then plotted.",
    br(),br(),
    fluidRow(
      column(
        3, style="border-right: 2px solid black",
        textAreaInput(NS(id, "genelist"),
                      HTML("List of gene names <br />
                            (Max 500 genes (over 50 will response slow), <br />
                             separated by , or ; or newline):"),
                      height = "200px",
                      value = NULL) %>%
          shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                              title = "List of genes to plot on bubbleplot / heatmap",
                              content = c("Input genes to plot",
                                          "- Maximum 500 genes (due to ploting space limitations)",
                                          "- Genes should be separated by comma, semicolon or newline")),
        xaxisCellInfoUI(id),
        subsetCellByInfoUI(id, mini=TRUE),
        sliderInput(NS(id, "filterVal"),
                    "Filter the cells by value",
                    min = 0, max = 10, value = 0),
        radioButtons(NS(id, "plottyp"), "Plot type:",
                     choices = c("Bubbleplot", "Heatmap"),
                     selected = "Bubbleplot", inline = TRUE),
        checkboxInput(NS(id, "plotscl"), "Scale gene expression", value = TRUE),
        checkboxInput(NS(id, "plotrow"), "Cluster rows (genes)", value = TRUE),
        checkboxInput(NS(id, "plotcol"), "Cluster columns (samples)", value = FALSE),
        checkboxInput(NS(id, "plotflp"),
                      "Flip X/Y", value = FALSE),
        br(),
        boxPlotControlUI(id, withPoints=FALSE, withColor=TRUE)
      ),
      column(
        9, h4(htmlOutput(NS(id, "oupTxt"))), geneExprDotPlotUI(id)
      )
    )
  )
}
plotBubbleHeatmapServer <- function(id, dataSource, optCrt, currentdataset){
  moduleServer(id, function(input, output, session){
    ## input column
    updateTextAreaInput(session, "genelist",
                        value = paste0(dataSource()$sc1def$genes,
                                       collapse = ", "))
    updateSelectInput(session,
                      "CellInfoX",
                      "Group by:",
                      choices = dataSource()$sc1conf[grp == TRUE]$UI,
                      selected = dataSource()$sc1def$grp1)
    updateSelectInput(session,
                      "subsetCell",
                      "Cell information to subset by:",
                      choices = c("N/A", dataSource()$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")


    ## update the ui
    output$subsetCell.ui <- renderUI({
      if(input$subsetCell!="N/A"){
        sub = strsplit(dataSource()$sc1conf[UI == input$subsetCell]$fID, "\\|")[[1]]
        checkboxGroupInput(NS(id, "subsetCellVal"),
                              "Select which cells to show",
                           inline = TRUE,
                           choices = sub,
                           selected = sub)
      }else{
        sub = NULL
      }
    })
    output$oupTxt <- renderUI({
      geneList = scGeneList(input$genelist, dataSource()$sc1gene)
      if(nrow(geneList) > 50){
        HTML("More than 50 input genes! Please reduce the gene list!")
      } else {
        oup = paste0(nrow(geneList[present == TRUE]), " genes OK and will be plotted")
        if(nrow(geneList[present == FALSE]) > 0){
          oup = paste0(oup, "<br/>",
                       nrow(geneList[present == FALSE]), " genes not found (",
                       paste0(geneList[present == FALSE]$gene, collapse = ", "), ")")
        }
        HTML(oup)
      }
    })

    ## plot region
    ### plots
    plot1 <- reactive({
      scBubbHeat(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$genelist,
        input$CellInfoX,
        input$subsetCell,
        input$subsetCellVal,
        input$filterVal,
        input$plottyp,
        dataSource()$dataset,
        "sc1gexpr.h5",
        dataSource()$sc1gene,
        input$plotscl,
        input$plotrow,
        input$plotcol,
        input$plotcols,
        input$plotflp,
        input$plotfsz,
        legendTitle=dataSource()$terms['expression'])
    })
    output$GeneExproup1 <- renderPlot({ plot1() })
    output$GeneExproup.ui1 <- renderUI({
      plotOutput(NS0(id, "GeneExproup", 1),
                 height = pList3[input$plotpsz])
    })
    output$GeneExproup.pdf1 <-
      plotsDownloadHandler(
        "pdf",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX)
    output$GeneExproup.png1 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX)
  })
}

