subsetPlotsUI <- function(id){
  ns <- NS(id)
  tabPanel(
    value=id,
    HTML("Explorer"),
    tabsubTitleUI(id, 'Plots',
                  description = paste(
                    "In this tab, users can visualise gene expression and",
                    "cell information by adding new module."
                  )),
    fluidRow(
      column(
        3,
        dimensionReductionUI(id)
      ),
      column(
        3,
        subsetCellByInfoUI(id)
      ),
      column(
        6,
        graphicsControlUI(id),
        subsetCellByFilterUI(id)
      )
    ),
    fluidRow(
      column(
        4,
      selectInput(ns('moduleName'), "Module type",
                  choices = c('cell info',
                              'gene expression',
                              'proportion',
                              'violin plot',
                              'boxplot',
                              'bubble plot',
                              'heatmap'),
                  selected = 'cell info')
      ),
      column(
        4,
      selectInput(ns('moduleWidth'), "Module width",
                  choices = c('half', 'full'),
                  selected = 'half')
      ),
      column(
        4,
        div(
          actionButton(ns("newModule"), "New Module",
                       icon = icon('plus'),
                       class="align-action-button"),
          div(style='display: none;',
              textInput(ns("removePlotModule"), '', value='', width=0))
        )
      )
    ),
    fluidRow(
      div(id=ns('container'),
          htmlOutput(ns('subplot')))
    )
  )
}
subsetPlotsServer <- function(id, dataSource, optCrt){
  moduleServer(id, function(input, output, session){
    ## subtitle
    output$PlotsSubTitle <-
      renderUI({
        h4(paste("Subset gene",
                 dataSource()$terms['expression'],
                 "on dimension reduction"))})
    ## input column 1
    ### Dimension Reduction
    updateDimRedSelInputPair(session, dataSource)
    ### Information to show

    ## input column 2
    updateSubsetCellUI(id, input, output, session, dataSource)
    updateFilterCellUI(id, optCrt, input, output, session, dataSource)

    ## plot region
    globals <- reactiveValues(
      containerIds = c(),
      containerUIs = list(),
      containerWidth = list()
    )
    uiGrid <- function(ui, width){
        column(width = width, ui)
    }
    getRown <- function(w){
      base <- 0
      currentLine <- 1
      l <- rep(0, length(w))
      for(i in seq_along(w)){
        if(w[i]==12){
          if(i==1){
            l[i] <- 1
          }else{
            l[i] <- l[i-1]+1
          }
          currentLine <- l[i] + 1
          base <- 0
        }else{
          base <- base + w[i]
          l[i] <- currentLine
          if(base==12){
            currentLine <- currentLine + 1
            base <- 0
          }
        }
      }
      return(l)
    }
    updatePlotModules <- function(){
      coln <- ifelse(globals$containerWidth[globals$containerIds]=="half",
                     6, 12)
      uis <- mapply(globals$containerUIs[globals$containerIds],
                    coln,
                    FUN = uiGrid,
                    SIMPLIFY = FALSE)
      uis <- lapply(split(uis, getRown(coln)), fluidRow)
      output$subplot <- renderUI(uis)
      scInfoServer(id, globals$containerIds,
                   dataSource,
                   optCrt,
                   input,
                   session)
    }
    observeEvent(input$newModule, {
      if(length(globals$containerIds)>=8){
        showNotification("Reached the limitation.", duration = 5,
                         closeButton = TRUE,
                         type = "warning")
      }else{
        ns0 <- paste0('plot_', length(globals$containerIds)+1)
        globals$containerIds <- c(globals$containerIds, ns0)
        globals$containerWidth[[ns0]] <- isolate(input$moduleWidth)
        globals$containerUIs[[ns0]] <- scInfoUI(NS(id, ns0))
        updatePlotModules()
      }
    })
    observeEvent(input$removePlotModule, {
      if(input$removePlotModule!=""){
        globals$containerIds <-
          globals$containerIds[globals$containerIds!=input$removePlotModule]
        globals$containerWidth[[input$removePlotModule]] <- NULL
        globals$containerUIs[[input$removePlotModule]] <- NULL
        updatePlotModules()
      }
    })
  })
}
