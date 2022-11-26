scVlnUI <- function(id, postfix=1){
  subModuleContainerUI(id,
                       mainSelectUI= selectInput(NS(id, "CellInfoX"),
                                                 "Cell information:",
                                                 choices = NULL),
                       menuUI= contextMenuViolinUI(id),
                       contentUI= geneExprDotPlotUI(id, postfix=postfix))
}
scVlnServer <- function(pid, id, dataSource, optCrt,
                         p_input, p_session, postfix=1){
  moduleServer(id, function(input, output, session){
    if(is.null(p_session$userData$defaults[[dataSource()$dataset]][[id]])){
      defaults <- list(
        CellInfoX = dataSource()$sc1def$grp2
      )
    }else{
      defaults <- p_session$userData$defaults[[dataSource()$dataset]][[id]]
    }
    ## title
    updateSelectInput(session,
                      "CellInfoX",
                      choices = getGroupUI(dataSource),
                      selected = defaults$CellInfoX)

    subModuleMenuObservor(id, input, p_session, dataSource,
                          c("CellInfoX", "plottyp", "plotpts"))
    ## plot
    plot1 <- reactive({
      scVioBox(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$CellInfoX,
        p_input$subsetCell,
        p_input$subsetCellVal,
        p_input$filterCellVal,
        p_input$filterCell,
        dataSource()$dataset,
        dataSource()$sc1gene,
        input$plottyp,
        input$plotpts,
        p_input$GeneExprsiz,
        p_input$GeneExprfsz)
    })
    updateSubModulePlotUI(postfix, pid, id, input, output, session,
                          plot1,
                          .globals$pList1[p_input$GeneExprpsz],
                          dataSource()$dataset,
                          input$plottyp,
                          input$CellInfoX)
  })
}
