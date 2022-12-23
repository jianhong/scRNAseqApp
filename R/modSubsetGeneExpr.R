subsetGeneExprUI <- function(id){
  tabPanel(
    value=id,
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
        selectInput(NS(id, "GeneName"),
                    "Gene name:",
                    choices=NULL) %>% helper1(cat="geneName"),
        selectInput(NS(id, "CellInfo"),
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
        graphicsControlUI(id), br(),
        cellInfoTblUI(id, 1)
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
subsetGeneExprServer <- function(id, dataSource, optCrt){
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
    updateDimRedSelInputPair(session, dataSource)
    ### Information to show
    #### gene name to plot
    updateSelectizeInput(session, "GeneName",
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
                      "CellInfo",
                      "Cell information to show:",
                      choices = dataSource()$sc1conf[
                        dataSource()$sc1conf$grp == TRUE]$UI,
                      selected = dataSource()$sc1def$grp1)

    ## input column 2
    updateSubsetCellUI(id, input, output, session, dataSource)
    updateFilterCellUI(id, optCrt, input, output, session, dataSource)

    ## plot region
    ### update the Color Range to make two plots comparable
    inpColRange <- reactive({
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName,
        input$CellInfo,
        c(input$GeneExprsub1b,input$GeneExprsub2b),
        dataSource()$dataset,
        dataSource()$sc1gene,
        input$GeneExprsiz,
        input$GeneExprcol1,
        input$GeneExprord1,
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input$GeneExprtype1,
        if(input$GeneExprxlimb1 %% 2==0) 0 else input$GeneExprxlim1,
        inpColRange=TRUE,
        infoFilterKey=input$subsetCell,
        infoFilterVal=input$subsetCellVal,
        valueFilterKey=input$filterCell,
        valueFilterCutoff=input$filterCellVal
      )
    })
    ## plots
    getSubGroupName <- function(dataSource, input){
      sub = strsplit(dataSource()$sc1conf[
        dataSource()$sc1conf$UI == input$CellInfo]$fID,
        "\\|")
      if(length(sub)){
        sub <- sub[[1]]
      }
      sub
    }
    updateSubsetGeneExprPlot(1, getSubGroupName, optCrt, inpColRange,
                             id, input, output, session, dataSource)
    updateSubsetGeneExprPlot(2, getSubGroupName, optCrt, inpColRange,
                             id, input, output, session, dataSource)

    ## cell stats
    observeEvent(input$GeneExprsub1b, updateGeneExprDT())
    observeEvent(input$GeneExprsub2b, updateGeneExprDT())
    updateGeneExprDT <- function(){
      output$GeneExpr.dt1 <- renderDT({
        ggData <- scDRnum(dataSource()$sc1conf,
                          dataSource()$sc1meta,
                          c(input$CellInfo,
                            input$GeneExprsub1b,
                            input$GeneExprsub2b),
                          input$GeneName,
                          input$subsetCell,
                          input$subsetCellVal,
                          dataSource()$dataset,
                          dataSource()$sc1gene,
                          input$GeneExprsplt1)
        datatable(ggData, rownames = FALSE, extensions = "Buttons",
                  options = list(pageLength = -1,
                                 dom = "tB",
                                 buttons = c("copy", "csv", "excel"))) %>%
          formatRound(columns = c("pctExpress"), digits = 2)
      })
    }
  })
}
