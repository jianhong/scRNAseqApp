scExprUI <- function(id, postfix=1){
  subModuleContainerUI(id,
                       mainSelectUI= selectInput(NS0(id, "GeneName", postfix),
                                                 "Gene name:", choices=NULL),
                       menuUI= contextMenuGeneExprUI(id, postfix),
                       contentUI= geneExprDotPlotUI(id, postfix))
}
scExprServer <- function(pid, id, dataSource, optCrt,
                         p_input, p_session, interactive,
                         postfix=1){
  moduleServer(id, function(input, output, session){
    ## title
    GeneNameLabel <- paste0('GeneName', postfix)
    if(is.null(p_session$userData$defaults[[dataSource()$dataset]][[id]])){
      defaults <- list()
      defaults[[GeneNameLabel]] <- dataSource()$sc1def$gene1
    }else{
      defaults <- p_session$userData$defaults[[dataSource()$dataset]][[id]]
    }
    updateSelectizeInput(session, GeneNameLabel,
                         choices = sort(names(dataSource()$sc1gene)),
                         server = TRUE,
                         selected = defaults[[GeneNameLabel]],
                         options = list(maxOptions = 6, create = TRUE,
                                        persist = TRUE, render = I(optCrt)))
    subModuleMenuObservor(id, input, p_session, dataSource,
                          c(GeneNameLabel,
                            paste0("GeneExprcol", postfix),
                            paste0("GeneExprord", postfix),
                            paste0("GeneExprtype", postfix),
                            paste0("GeneExprxlim", postfix),
                            paste0("GeneExprrg", postfix)))
    ### plots
    plotX <- reactive({
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        p_input$GeneExprdrX,
        p_input$GeneExprdrY,
        input[[GeneNameLabel]],
        p_input$subsetCell,
        p_input$subsetCellVal,
        dataSource()$dataset,
        dataSource()$sc1gene,
        p_input$GeneExprsiz,
        input[[paste0("GeneExprcol", postfix)]],
        input[[paste0("GeneExprord", postfix)]],
        p_input$GeneExprfsz,
        p_input$GeneExprasp,
        p_input$GeneExprtxt,
        input[[paste0("GeneExprtype", postfix)]],
        if(input[[paste0("GeneExprxlimb", postfix)]] %% 2==0) 0 else
          input[[paste0("GeneExprxlim", postfix)]],
        inpColRange=if(input[[paste0("GeneExprrgb", postfix)]] %% 2==0) 0 else
          input[[paste0("GeneExprrg", postfix)]],
        valueFilterKey=p_input$filterCell,
        valueFilterCutoff=p_input$filterCellVal)
    })
    updateSubModulePlotUI(postfix, pid, id, input, output, session,
                          interactive,
                          plotX,
                          .globals$pList1[p_input$GeneExprpsz],
                          dataSource()$dataset,
                          p_input$GeneExprdrX,
                          p_input$GeneExprdrY,
                          input[[GeneNameLabel]])
  })
}
