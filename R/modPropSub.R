scPropUI <- function(id, postfix=1){
  subModuleContainerUI(id,
                       mainSelectUI= selectInput(NS(id, "CellInfoX"),
                                                 "Cell information:",
                                                 choices = NULL),
                       menuUI= contextMenuPropUI(id),
                       contentUI= geneExprDotPlotUI(id, postfix=postfix))
}
scPropServer <- function(pid, id, dataSource, optCrt,
                         p_input, p_session, postfix=1){
  moduleServer(id, function(input, output, session){
    if(is.null(p_session$userData$defaults[[dataSource()$dataset]][[id]])){
      defaults <- list(
        CellInfoX = dataSource()$sc1def$grp2,
        cellInfoY = dataSource()$sc1def$grp1
      )
    }else{
      defaults <- p_session$userData$defaults[[dataSource()$dataset]][[id]]
    }
    ## input column
    updateSelectInput(session,
                      "CellInfoX",
                      choices = getGroupUI(dataSource),
                      selected = defaults$cellinfoX)
    updateSelectInput(session,
                      "cellInfoY",
                      choices = getGroupUI(dataSource),
                      selected = defaults$cellinfoY)

    subModuleMenuObservor(id, input, p_session, dataSource,
                          c("CellInfoX", "CellInfoY",
                            "plottyp", "plotflp"))

    ## plot region
    ### plots
    plot1 <- reactive({
      scProp(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$CellInfoX,
        p_input$subsetCell,
        p_input$subsetCellVal,
        input$cellInfoY,
        input$plottyp,
        input$plotflp,
        p_input$GeneExprfsz,
        dataset=dataSource()$dataset,
        geneIdMap=dataSource()$sc1gene,
        valueFilterKey=p_input$filterCell,
        valueFilterCutoff=p_input$filterCellVal)
    })

    updateSubModulePlotUI(postfix, pid, id, input, output, session,
                          plot1,
                          .globals$pList1[p_input$GeneExprpsz],
                          dataSource()$dataset,
                          input$plottyp,
                          input$CellInfoX,
                          input$cellInfoY)
  })
}
