closeUI <- function(id){
  div(
    class="shinyhelper-wrapper",
    div(class='shinyhelper-container about-margin-right',
        actionButton(NS(id, 'close'),
                     label = '',
                     icon = icon('close')))
  )
}

scInfoUI <- function(id, postfix=1){
  div(
    class='about-container-black-border',
    fluidRow(
      column(5, cellInfoUI(id, postfix)),
      column(6, cellInfoPlotControlUI(id, postfix)),
      column(1, closeUI(id))
    ),
    geneExprDotPlotUI(id, postfix)
  )
}
scInfoServer <- function(pid, ids, dataSource, optCrt,
                         p_input, p_session, postfix=1){
  lapply(ids, function(id){
    moduleServer(id, function(input, output, session){
      ## plot region
      ### dropdown list
      cellInfoLabel <- paste0('CellInfo', postfix)
      updateSelectInput(session, cellInfoLabel, "Cell information:",
                        choices = dataSource()$sc1conf$UI,
                        selected = dataSource()$sc1def$meta1)
      observeEvent(input$close, {
        updateTextInput(p_session, "removePlotModule", value = id)
      })
      plotX <- reactive({
        scDRcell(
          dataSource()$sc1conf,
          dataSource()$sc1meta,
          p_input$GeneExprdrX,
          p_input$GeneExprdrY,
          input[[cellInfoLabel]],
          p_input$subsetCell,
          p_input$subsetCellVal,
          p_input$GeneExprsiz,
          input[[paste0("CellInfocol", postfix)]],
          input[[paste0("CellInfoord", postfix)]],
          p_input$GeneExprfsz,
          p_input$GeneExprasp,
          p_input$GeneExprtxt,
          input[[paste0("CellInfolab", postfix)]],
          input[[paste0("CellInfoslingshot", postfix)]],
          file.path(.globals$datafolder,
                    dataSource()$dataset,
                    .globals$filenames[["slingshot"]]))
      })
      updatePlotUI(postfix, pid, id, input, output, session,
                   plotX,
                   .globals$pList1[p_input$GeneExprpsz],
                   dataSource()$dataset,
                   p_input$GeneExprdrX,
                   p_input$GeneExprdrY,
                   input[[cellInfoLabel]])
    })
  })
}

updatePlotUI <-
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
