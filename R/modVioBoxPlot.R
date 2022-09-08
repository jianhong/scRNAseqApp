plotVioBoxUI <- function(id){
  tabPanel(
    HTML("Violinplot / Boxplot"),
    h4("Cell information / gene expression violin plot / box plot"),
    "In this tab, users can visualise the gene expression or continuous cell information ",
    "(e.g. Number of UMIs / module score) across groups of cells (e.g. libary / clusters).",
    br(),br(),
    fluidRow(
      column(
        3, style="border-right: 2px solid black",
        xaxisCellInfoUI(id),
        subsetCellByInfoUI(id, mini=TRUE),
        subsetCellByFilterUI(id, label = "Cell Info / Gene name (Y-axis):",
                             title = "Cell Info / Gene to plot",
                             content = c("Select cell info / gene to plot on Y-axis",
                                         "- Can be continuous cell information (e.g. nUMIs / scores)",
                                         "- Can also be gene expression")),
        radioButtons(NS(id, "plottyp"), "Plot type:",
                     choices = c("violin", "boxplot"),
                     selected = "violin", inline = TRUE),
        checkboxInput(NS(id, "plotpts"), "Show data points", value = FALSE),
        boxPlotControlUI(id)
      ),
      column(
        9, geneExprDotPlotUI(id)
      )
    )
  )
}
plotVioBoxServer <- function(id, dataSource, optCrt, currentdataset, datafolder){
  moduleServer(id, function(input, output, session){
    ## input column
    updateSelectInput(session,
                      "CellInfoX",
                      "Cell information (X-axis):",
                      choices = dataSource()$sc1conf[grp == TRUE]$UI,
                      selected = dataSource()$sc1def$grp1)
    updateSelectInput(session,
                      "subsetCell",
                      "Cell information to subset by:",
                      choices = c("N/A", dataSource()$sc1conf[grp == TRUE]$UI),
                      selected = "N/A")
    updateSelectizeInput(session,
                         "filterCell",
                         server = TRUE,
                         choices = c(dataSource()$sc1conf[is.na(fID)]$UI,
                                     sort(names(dataSource()$sc1gene))),
                         selected = dataSource()$sc1conf[is.na(fID)]$UI[1],
                         options = list(
                           maxOptions = length(dataSource()$sc1conf[is.na(fID)]$UI) + 3,
                           create = TRUE, persist = TRUE, render = I(optCrt)))


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
    output$filterCell.ui <- renderUI({
      if(!input$filterCell %in% dataSource()$sc1conf$UI){
        h5file <- H5File$new(file.path(datafolder, dataSource()$dataset, "sc1gexpr.h5"), mode = "r")
        h5data <- h5file[["grp"]][["data"]]
        val = h5data$read(args = list(dataSource()$sc1gene[input$filterCell], quote(expr=)))
        val <- max(val, na.rm = TRUE)
        h5file$close_all()
      }else{
        val = dataSource()$sc1meta[[dataSource()$sc1conf[UI == input$filterCell]$ID]]
        val <- max(val, na.rm = TRUE)
      }
      if(val<=1) maxv <- round(val, digits = 3)
      if(val>1 && val<=10) maxv <- round(val, digits = 1)
      if(val>10) maxv <- round(val, digits = 0)
      sliderInput(NS(id, "filterCellVal"),
                  "Filter the cells by value",
                  min = 0, max = maxv, value = 0)
    })

    ## plot region
    ### plots
    plot1 <- reactive({
      scVioBox(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$CellInfoX,
        input$subsetCell,
        input$subsetCellVal,
        input$filterCellVal,
        input$filterCell,
        dataSource()$dataset,
        "sc1gexpr.h5",
        dataSource()$sc1gene,
        input$plottyp,
        input$plotpts,
        input$plotsiz,
        input$plotfsz)
    })
    output$GeneExproup1 <- renderPlot({ plot1() })
    output$GeneExproup.ui1 <- renderUI({
      plotOutput(NS0(id, "GeneExproup", 1),
                 height = pList2[input$plotpsz])
    })
    output$GeneExproup.pdf1 <-
      plotsDownloadHandler(
        "pdf",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX,
        input$filterCell)
    output$GeneExproup.png1 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$plottyp,
        input$CellInfoX,
        input$filterCell)
  })
}

