updateDimRedSelInput <-
  function(session, inputId, label, conf, selected){
    updateSelectInput(session, inputId, label,
                      choices = conf[conf$dimred == TRUE]$UI,
                      selected = selected)
  }
updateDimRedSelInputPair <-
  function(session, dataSource){
    updateDimRedSelInput(session, "GeneExprdrX", "X-axis:",
                         dataSource()$sc1conf,
                         dataSource()$sc1def$dimred[1])
    updateDimRedSelInput(session, "GeneExprdrY", "Y-axis:",
                         dataSource()$sc1conf,
                         dataSource()$sc1def$dimred[2])
  }
getGroupUI <- function(dataSource){
  dataSource()$sc1conf[
    dataSource()$sc1conf$grp == TRUE]$UI
}
updateSubsetCellUI <-
  function(id, input, output, session, dataSource, addNA=FALSE){
    choices <- dataSource()$sc1conf[
      dataSource()$sc1conf$grp == TRUE]$UI
    if(addNA){
      selected  <- "N/A"
      choices <- c("N/A", choices)
    }else{
      selected <- dataSource()$sc1def$grp1
    }
    updateSelectInput(session,
                      "subsetCell",
                      "Cell information to subset:",
                      choices = choices,
                      selected = selected)

    output$subsetCell.ui <- renderUI({
      if(input$subsetCell!="N/A"){
        x <- dataSource()$sc1conf[
          dataSource()$sc1conf$UI == input$subsetCell]$fID
        if(!is.null(x)){
          sub <- strsplit(dataSource()$sc1conf[
            dataSource()$sc1conf$UI == input$subsetCell]$fID,
            "\\|")[[1]]
          div(
            style="max-height: 150px; display:flex; flex-direction: column; overflow-y: auto;",
            checkboxGroupInput(NS(id, "subsetCellVal"),
                               "Select which cells to show",
                               inline = TRUE,
                               choices = sub,
                               selected = sub)
          )
        }
      }
    })
  }
updateFilterCellUI <-
  function(id, optCrt, input, output, session, dataSource){
    updateSelectizeInput(session,
                         "filterCell",
                         server = TRUE,
                         choices = c(dataSource()$sc1conf[
                           is.na(dataSource()$sc1conf$fID)]$UI,
                           sort(names(dataSource()$sc1gene))),
                         selected = dataSource()$sc1conf[
                           is.na(dataSource()$sc1conf$fID)]$UI[1],
                         options = list(
                           maxOptions =
                             length(dataSource()$sc1conf[
                               is.na(dataSource()$sc1conf$fID)]$UI) + 3,
                           create = TRUE,
                           persist = TRUE,
                           render = I(optCrt)))
    output$filterCell.ui <- renderUI({
      if(!input$filterCell %in% dataSource()$sc1conf$UI){
        val <- read_exprs(dataSource()$dataset,
                          dataSource()$sc1gene[input$filterCell],
                          valueOnly=TRUE)
      }else{
        val = dataSource()$sc1meta[[
          dataSource()$sc1conf[
            dataSource()$sc1conf$UI == input$filterCell]$ID]]
      }
      val <- max(val, na.rm = TRUE)
      if(val<=1) maxv <- round(val, digits = 3)
      if(val>1 && val<=10) maxv <- round(val, digits = 1)
      if(val>10) maxv <- round(val, digits = 0)
      sliderInput(NS(id, "filterCellVal"),
                  "Filter the cells by value",
                  min = 0, max = maxv, value = 0)
    })
  }

updateGeneExprDotPlotUI <-
  function(postfix=1, id, input, output, session, plotX, height, ...){
    output[[paste0("GeneExproup", postfix)]] <- renderPlot({ plotX() })
    output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
      plotOutput(NS0(id, "GeneExproup", postfix),
                 height = height)
    })
    output[[paste0("GeneExproup.pdf", postfix)]] <-
      plotsDownloadHandler(
        "pdf",
        width=input[[paste0("GeneExproup.w", postfix)]],
        height=input[[paste0("GeneExproup.h", postfix)]],
        plotX(),
        ...)
    output[[paste0("GeneExproup.png", postfix)]] <-
      plotsDownloadHandler(
        "png",
        width=input[[paste0("GeneExproup.w", postfix)]],
        height=input[[paste0("GeneExproup.h", postfix)]],
        plotX(),
        ...)
  }

updateCellInfoPlot <-
  function(postfix=1, id, input, output, session, dataSource){
    cellInfoLabel <- paste0('CellInfo', postfix)
    updateSelectInput(session, cellInfoLabel, "Cell information:",
                      choices = dataSource()$sc1conf$UI,
                      selected = dataSource()$sc1def[[paste0("meta", postfix)]])
    plotX <- reactive({
      scDRcell(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input[[cellInfoLabel]],
        input$subsetCell,
        input$subsetCellVal,
        input$GeneExprsiz,
        input[[paste0("CellInfocol", postfix)]],
        input[[paste0("CellInfoord", postfix)]],
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input[[paste0("CellInfolab", postfix)]],
        input[[paste0("CellInfoslingshot", postfix)]],
        file.path(.globals$datafolder,
                  dataSource()$dataset,
                  .globals$filenames[["slingshot"]]))
    })
    updateGeneExprDotPlotUI(postfix, id, input, output, session,
                            plotX,
                            .globals$pList1[input$GeneExprpsz],
                            dataSource()$dataset,
                            input$GeneExprdrX,
                            input$GeneExprdrY,
                            input[[cellInfoLabel]])
}

updateGeneExprPlot <-
  function(postfix=1, selectedGene, optCrt,
           id, input, output, session, dataSource){
    GeneNameLabel <- paste0('GeneName', postfix)
    updateSelectizeInput(session, GeneNameLabel,
                         choices = sort(names(dataSource()$sc1gene)),
                         server = TRUE,
                         selected = selectedGene,
                         options = list(maxOptions = 6, create = TRUE,
                                        persist = TRUE, render = I(optCrt)))
    ### plots
    plotX <- reactive({
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input[[GeneNameLabel]],
        input$subsetCell,
        input$subsetCellVal,
        dataSource()$dataset,
        dataSource()$sc1gene,
        input$GeneExprsiz,
        input[[paste0("GeneExprcol", postfix)]],
        input[[paste0("GeneExprord", postfix)]],
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input[[paste0("GeneExprtype", postfix)]],
        if(input[[paste0("GeneExprxlimb", postfix)]] %% 2==0) 0 else
          input[[paste0("GeneExprxlim", postfix)]],
        inpColRange=if(input[[paste0("GeneExprrgb", postfix)]] %% 2==0) 0 else
          input[[paste0("GeneExprrg", postfix)]])
    })
    updateGeneExprDotPlotUI(postfix, id, input, output, session,
                            plotX,
                            .globals$pList1[input$GeneExprpsz],
                            dataSource()$dataset,
                            input$GeneExprdrX,
                            input$GeneExprdrY,
                            input[[GeneNameLabel]])
  }

updateSubsetGeneExprPlot <-
  function(postfix=1, subgrp, optCrt, inpColRange,
           id, input, output, session, dataSource){
    GeneNameLabel <- paste0("GeneExprsub", postfix, "b")
    ### sub region title
    output[[paste0("subPlotTitle", postfix)]] <-
      renderUI({h4(paste("Gene", dataSource()$terms['expression']))})
    ### select which cells to show
    output[[paste0("GeneExprgrp.ui", postfix)]] <- renderUI({
      subgrp <- subgrp(dataSource, input)
      selected <- ifelse(postfix==1, subgrp[1],
                         ifelse(length(subgrp)>1, subgrp[2], subgrp[1]))
      checkboxGroupInput(NS(id, GeneNameLabel),
                         "Select which cells to show",
                         inline = TRUE,
                         choices = subgrp,
                         selected = selected)
    })
    ### plots
    plotX <- reactive({
      scDRgene(
        dataSource()$sc1conf,
        dataSource()$sc1meta,
        input$GeneExprdrX,
        input$GeneExprdrY,
        input$GeneName,
        input$CellInfo,
        input[[GeneNameLabel]],
        dataSource()$dataset,
        dataSource()$sc1gene,
        input$GeneExprsiz,
        input[[paste0("GeneExprcol", postfix)]],
        input[[paste0("GeneExprord", postfix)]],
        input$GeneExprfsz,
        input$GeneExprasp,
        input$GeneExprtxt,
        input[[paste0("GeneExprtype", postfix)]],
        if(input[[paste0("GeneExprxlimb", postfix)]] %% 2==0) 0 else
                         input[[paste0("GeneExprxlim", postfix)]],
        inpColRange=if(input[[paste0("GeneExprrgb", postfix)]] %% 2==0){
          inpColRange()
        }else{ input[[paste0("GeneExprrg", postfix)]] },
        inpsub3=input$subsetCell,
        inpsub3filter=input$subsetCellVal,
        inpsub4=input$filterCell,
        inpsub4filter=input$filterCellVal)
    })
    updateGeneExprDotPlotUI(postfix, id, input, output, session,
                            plotX,
                            .globals$pList1[input$GeneExprpsz],
                            dataSource()$dataset,
                            input$GeneExprdrX,
                            input$GeneExprdrY,
                            input$GeneName,
                            input$CellInfo)
  }

# sub module related
updateSubModulePlotUI <-
  function(postfix=1, pid, id, input, output, session, plotX, height, ...){
    output[[paste0("GeneExproup", postfix)]] <- renderPlot({ plotX() })
    output[[paste0("GeneExproup.ui", postfix)]] <- renderUI({
      plotOutput(NS0(NS(pid, id), "GeneExproup", postfix),
                 height = height)
    })
    output[[paste0("GeneExproup.pdf", postfix)]] <-
      plotsDownloadHandler(
        "pdf",
        width=input[[paste0("GeneExproup.w", postfix)]],
        height=input[[paste0("GeneExproup.h", postfix)]],
        plotX(),
        ...)
    output[[paste0("GeneExproup.png", postfix)]] <-
      plotsDownloadHandler(
        "png",
        width=input[[paste0("GeneExproup.w", postfix)]],
        height=input[[paste0("GeneExproup.h", postfix)]],
        plotX(),
        ...)
  }

subModuleMenuObservor <- function(id, input, p_session, dataSource,
                                  observeEvtList){
  observeEvent(input$close, {
    updateTextInput(p_session, "removePlotModule", value = id)
  })
  observeEvent(input$movedown, {
    updateTextInput(p_session, "movedownPlotModule", value = id)
  })
  observeEvent(input$moveup, {
    updateTextInput(p_session, "moveupPlotModule", value = id)
  })
  observeEvent(input$resize, {
    updateTextInput(p_session, "resizePlotModule", value = id)
  })
  if(is.null(p_session$userData$defaults[[dataSource()$dataset]][[id]]))
    p_session$userData$defaults[[dataSource()$dataset]][[id]] <- list()
  lapply(observeEvtList, function(evt){
    observeEvent(input[[evt]], {
      p_session$userData$defaults[[dataSource()$dataset]][[id]][[evt]] <-
        input[[evt]]
    })
  })
}
