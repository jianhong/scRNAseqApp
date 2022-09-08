geneExprGeneExprUI <- function(id){
  tabPanel(
    htmlOutput(NS(id, 'GeneExpr')),
    tabsubTitleUI(id, 'GeneExpr',
                  description = paste(
                    "In this tab, users can visualise two gene expressions ",
                    "side-by-side on low-dimensional representions."
                  )),
    fluidRow(
      column(3, dimensionReductionUI(id)),
      column(3, subsetCellByInfoUI(id)),
      column(6, graphicsControlUI(id))
    ),
    fluidRow(
      column(
        6, style="border-right: 2px solid black",
        htmlOutput(NS0(id, "subPlotTitle", 1)),
        fluidRow(
          column(6, geneExprUI(id, 1)),
          column(6, geneExprPlotControlUI(id, 1))
        ),
        geneExprDotPlotUI(id, 1)
      ),
      column(
        6, htmlOutput(NS0(id, "subPlotTitle", 2)),
        fluidRow(
          column(6, geneExprUI(id, 2)),
          column(6, geneExprPlotControlUI(id, 2))
        ),
        geneExprDotPlotUI(id, 2)
      )
    )
  )
}
geneExprGeneExprServer <- function(id, dataSource, optCrt, currentdataset){
  moduleServer(id, function(input, output, session){
    ## title
    output$GeneExpr <-
      renderUI({HTML(paste(dataSource()$terms["GeneExpr"], "vs",
                           dataSource()$terms["GeneExpr"]))})
    ## subtitle
    output$GeneExprSubTitle <-
      renderUI({
        h4(paste("Gene", dataSource()$terms['expression'],
                 "vs gene", dataSource()$terms['expression'],
                 "on dimension reduction"))})
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
      renderUI({h4(paste("Gene", dataSource()$terms['expression'], "1"))})
    output$subPlotTitle2 <-
      renderUI({h4(paste("Gene", dataSource()$terms['expression'], "2"))})
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
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName1,
        input$subsetCell,
        input$subsetCellVal,
        dataSource()$dataset,
        "sc1gexpr.h5",
        dataSource()$sc1gene,
        input$GeneExprsiz,
        input$GeneExprcol1,
        input$GeneExprord1,
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input$GeneExprtype1,
        if(input$GeneExprxlimb1 %% 2==0) 0 else input$GeneExprxlim1,
        inpColRange=if(input$GeneExprrgb1 %% 2==0) 0 else input$GeneExprrg1)
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
        input$GeneName1)
    output$GeneExproup.png1 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName1)

    plot2 <- reactive({
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName2,
        input$subsetCell,
        input$subsetCellVal,
        dataSource()$dataset,
        "sc1gexpr.h5",
        dataSource()$sc1gene,
        input$GeneExprsiz,
        input$GeneExprcol2,
        input$GeneExprord2,
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input$GeneExprtype2,
        if(input$GeneExprxlimb2 %% 2==0) 0 else input$GeneExprxlim2,
        inpColRange=if(input$GeneExprrgb2 %% 2==0) 0 else input$GeneExprrg2)
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
        input$GeneName2)
  })
}

