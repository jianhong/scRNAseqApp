subsetGeneExprUI <- function(id){
  tabPanel(
    htmlOutput(NS(id, 'GeneExpr')),
    tabsubTitleUI(id, 'GeneExpr',
                  description = paste(
                    "In this tab, users can visualise gene expressions of",
                    "two groups side-by-side on low-dimensional representions."
                  )),
    fluidRow(
      column(
        3,
        dimensionReductionUI(id),
        h4("Information to show"),
        selectInput(NS(id, "GeneExprinp1"),
                    "Gene name:",
                    choices=NULL) %>% helper1(cat="geneName"),
        selectInput(NS(id, "GeneExprinp2"),
                    "Cell information to show:",
                    choices = NULL)
      ),
      column(
        3,
        subsetCellByInfoUI(id),
        subsetCellByFilterUI(id)
      ),
      column(
        6,
        graphicsControlUI(id)
      )
    ),
    fluidRow(
      column(
        6, style="border-right: 2px solid black",
        htmlOutput(NS0(id, "subPlotTitle", 1)),
        fluidRow(
          column(
            6, uiOutput(NS0(id, "GeneExprgrp.ui", 1))
          ),
          column(
            6, geneExprPlotControlUI(id, postfix = 1)
          )
        ),
        geneExprDotPlotUI(id, 1)
      ),
      column(
        6, htmlOutput(NS0(id, "subPlotTitle", 2)),
        fluidRow(
          column(
            6, uiOutput(NS0(id, "GeneExprgrp.ui", 2))
          ),
          column(
            6, geneExprPlotControlUI(id, postfix = 2)
          )
        ),
        geneExprDotPlotUI(id, 2)
      )
    )
  )
}
subsetGeneExprServer <- function(id, dataSource, optCrt, currentdataset){
  moduleServer(id, function(input, output, session){
    ## title
    output$GeneExpr <-
      renderUI({HTML(paste("Subset", dataSource()$terms["GeneExpr"]))})
    ## subtitle
    output$GeneExprSubTitle <-
      renderUI({
        h4(paste("Subset gene",
                 dataSource()$terms['expression'],
                 "on dimension reduction"))})
    ## input column 1
    ### Dimension Reduction
    updateSelectInput(session, "GeneExprdrX", "X-axis:",
                      choices = dataSource()$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource()$sc1def$dimred[1])
    updateSelectInput(session,"GeneExprdrY", "Y-axis:",
                      choices = dataSource()$sc1conf[dimred == TRUE]$UI,
                      selected = dataSource()$sc1def$dimred[2])
    ### Information to show
    #### gene name to plot
    updateSelectizeInput(session, "GeneExprinp1",
                         choices = sort(names(dataSource()$sc1gene)),
                         server = TRUE,
                         selected = dataSource()$sc1def$gene1,
                         options = list(
                           maxOptions = 6,
                           create = TRUE,
                           persist = TRUE,
                           render = I(optCrt)))
    #### cell information to plot
    updateSelectInput(session,
                      "GeneExprinp2",
                      "Cell information to show:",
                      choices = dataSource()$sc1conf[grp == TRUE]$UI,
                      selected = dataSource()$sc1def$grp1)
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

    updateSelectizeInput(session,
                         "filterCell",
                         server = TRUE,
                         choices = c(dataSource()$sc1conf[is.na(fID)]$UI,
                                     sort(names(dataSource()$sc1gene))),
                         selected = dataSource()$sc1conf[is.na(fID)]$UI[1],
                         options = list(
                           maxOptions =
                             length(dataSource()$sc1conf[is.na(fID)]$UI) + 3,
                           create = TRUE,
                           persist = TRUE,
                           render = I(optCrt)))
    output$filterCell.ui <- renderUI({
      if(!input$filterCell %in% dataSource()$sc1conf$UI){
        h5file <- H5File$new(file.path(datafolder,
                                       dataSource()$dataset,
                                       "sc1gexpr.h5"),
                             mode = "r")
        h5data <- h5file[["grp"]][["data"]]
        val = h5data$read(
          args = list(dataSource()$sc1gene[input$filterCell],
                      quote(expr=)))
        val <- max(val, na.rm = TRUE)
        h5file$close_all()
      }else{
        val = dataSource()$sc1meta[[
          dataSource()$sc1conf[UI == input$filterCell]$ID]]
        val <- max(val, na.rm = TRUE)
      }
      if(val<=1) maxv <- round(val, digits = 3)
      if(val>1 && val<=10) maxv <- round(val, digits = 1)
      if(val>10) maxv <- round(val, digits = 0)
      sliderInput(NS(id, "filterCellVal"), "Filter the cells by value",
                  min = 0, max = maxv, value = 0)
    })

    ## plot region
    ### sub region title
    output$subPlotTitle1 <-
      renderUI({h4(paste("Gene", dataSource()$terms['expression']))})
    output$subPlotTitle2 <-
      renderUI({h4(paste("Gene", dataSource()$terms['expression']))})
    ### select which cells to show
    output$GeneExprgrp.ui1 <- renderUI({
      sub = strsplit(dataSource()$sc1conf[UI==input$GeneExprinp2]$fID,
                     "\\|")[[1]]
      checkboxGroupInput(NS(id, "GeneExprsub1b"),
                         "Select which cells to show",
                         inline = TRUE,
                         choices = sub,
                         selected = sub[1])
    })

    output$GeneExprgrp.ui2 <- renderUI({
      sub = strsplit(dataSource()$sc1conf[UI == input$GeneExprinp2]$fID,
                     "\\|")
      if(length(sub)){
        sub <- sub[[1]]
      }
      checkboxGroupInput(NS(id, "GeneExprsub2b"),
                         "Select which cells to show",
                         inline = TRUE,
                         choices = sub,
                         selected = ifelse(length(sub)>1, sub[2], sub[1]))
    })
    plot1 <- reactive({
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneExprinp1,
        input$GeneExprinp2,
        input$GeneExprsub1b,
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
        inpColRange=if(input$GeneExprrgb1 %% 2==0) 0 else input$GeneExprrg1,
        inpsub3=input$subsetCell,
        inpsub3filter=input$subsetCellVal,
        inpsub4=input$filterCell,
        inpsub4filter=input$filterCellVal)
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
        input$GeneExprinp1,
        input$GeneExprinp2)
    output$GeneExproup.png1 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w1,
        height=input$GeneExproup.h1,
        plot1(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneExprinp1,
        input$GeneExprinp2)
    plot2 <- reactive({
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneExprinp1,
        input$GeneExprinp2,
        input$GeneExprsub2b,
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
        inpColRange=if(input$GeneExprrgb2 %% 2==0) 0 else input$GeneExprrg2,
        inpsub3=input$subsetCell,
        inpsub3filter=input$subsetCellVal,
        inpsub4=input$filterCell,
        inpsub4filter=input$filterCellVal)
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
        input$GeneExprinp1,
        input$GeneExprinp2)
    output$GeneExproup.png2 <-
      plotsDownloadHandler(
        "png",
        width=input$GeneExproup.w2,
        height=input$GeneExproup.h2,
        plot2(),
        currentdataset,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneExprinp1,
        input$GeneExprinp2)
  })
}
