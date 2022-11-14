plotBubbleHeatmapUI <- function(id){
  tabPanel(
    value = id,
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
        conditionalPanel(
          condition = paste0("input.plottyp == 'Heatmap'"), ns=NS(id),
          checkboxInput(NS(id, "plotall"), "All cells (time consuming)", value = FALSE)
          ),
        actionButton(NS(id, "userbreaks"),
                     "ColorKey range"),
        conditionalPanel(
          condition = paste0("input.userbreaks % 2 ==1"),
          ns=NS(id),
          numericInput(NS(id, "colorb1"), "min cutoff", value = -10),
          numericInput(NS(id, "colorb2"), "max cutoff", value = 10),
          actionButton(NS(id, "setcolrg"), "Apply colorkey range")
        ),
        br(),
        boxPlotControlUI(id, withPoints=FALSE, withColor=TRUE)
      ),
      column(
        9, h4(htmlOutput(NS(id, "oupTxt"))), geneExprDotPlotUI(id)
      )
    )
  )
}

plotBubbleHeatmapServer <- function(id, dataSource, optCrt, currentdataset,
                                    datafolder){
  moduleServer(id, function(input, output, session){
    colrg <- reactiveVal(NA)
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
                        value = paste0(genelist,
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

    observeEvent(input$userbreaks, {
      rg <- scBubbHeat(
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
        input$plotall,
        legendTitle=dataSource()$terms['expression'],
        datafolder=datafolder,
        returnColorRange=TRUE)
      rg <- round(rg, digits = 2)
      updateNumericInput(session, "colorb1", value = rg[2],
                         min = floor(rg[1]), max = ceiling(rg[4]),
                         step = .01)
      updateNumericInput(session, "colorb2", value = rg[3],
                         min = ceiling(rg[1]), max=ceiling(rg[4]),
                         step = .01)

    })
    observeEvent(input$setcolrg, {
      colrg(sort(c(input$colorb1, input$colorb2)))
    })

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
      ht <- scBubbHeat(
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
        input$plotall,
        colorBreaks=if(input$userbreaks %% 2==0 && !is.na(colrg()[1])) NA else colrg(),
        legendTitle=dataSource()$terms['expression'],
        datafolder=datafolder)
    })
    output$GeneExproup1 <- renderPlot({ plot1() })
    output$GeneExproup.ui1 <- renderUI({
      plotOutput(NS0(id, "GeneExproup", 1),
                 height = .globals$pList3[input$plotpsz])
    })
    output$GeneExproup.pdf1 <-
      heatmapDownloadHandler(
        "pdf",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX)
    output$GeneExproup.png1 <-
      heatmapDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX)
  })
}

