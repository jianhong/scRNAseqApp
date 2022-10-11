#' scRNAseqApp main function
#' @param datafolder the folder where saved the dataset for the app
#' @param defaultDataset default dataset for the app.
#' @param windowTitle The title that should be displayed by the browser window.
#' @param ... parameters can be passed to shinyApp except ui and server.
#' @import shiny
#' @importFrom utils packageVersion read.delim
#' @importFrom shinyhelper observe_helpers
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal xlab ylab
#' @export
#' @examples
#' if(interactive()){
#'   app_path=tempdir()
#'   scInit(app_path=app_path)
#'   setwd(app_path)
#'   scRNAseqApp()
#' }
scRNAseqApp <- function(datafolder = "data",
                        defaultDataset = "pbmc_small",
                        windowTitle = "scRNAseq/scATACseq database",
                        ...){
  ## load default parameters
  loginNavbarTitle <- "Switch User"
  datasets <- getDataSets(datafolder = datafolder)
  defaultDataset <- getDefaultDataset(defaultDataset=defaultDataset,
                                      datafolder = datafolder)
  appconf <- getAppConf(datafolder = datafolder)
  datasets <- getDataSets(datafolder = datafolder, appconf = appconf)
  data_types <- getDataType(appconf = appconf)

  ui0 <- function(req){
    fluidPage(
      ### HTML formatting of error messages
      tags$head(
        tags$style(HTML(".shiny-output-error-validation {color: red; font-weight: bold;}")),
        tags$style(HTML(".rightAlign{float:right;}")),
        tags$style(HTML(".navbar-default .navbar-nav { font-weight: bold; font-size: 16px; }"))
      ),

      navbarPage(
        selectInput('availableDatasets',
                    label = NULL,
                    choices = datasets,
                    selected = defaultDataset,
                    width = "90vw"),
        windowTitle = windowTitle,
        id = "topnav",
        footer = div(p(em(windowTitle, " (Version:",
                          as.character(packageVersion("scRNAseqApp")), ")"),
                       HTML("&copy;"), "2020 -",
                       format(Sys.Date(), "%Y"),
                       "jianhong@duke"), class="rightAlign"),
        ### Tab: cellInfo vs geneExpr on dimRed
        cellInfoGeneExprUI("cellInfoGeneExpr"),
        ### Tab: cellInfo vs cellInfo on dimRed
        cellInfoCellInfoUI("cellInfoCellInfo"),
        ### Tab: geneExpr vs geneExpr on dimRed
        geneExprGeneExprUI("geneExprGeneExpr"),
        ### Tab: Gene coexpression plot
        coExprUI("coExpr"),
        ### Tab: subset gene expr
        subsetGeneExprUI("subsetGeneExpr"),
        ### Tab: violinplot / boxplot
        plotVioBoxUI("vioBoxPlot"),
        ### Tab: Proportion plot
        plotProportionUI("proportion"),
        ### Tab: Multiple gene expr
        plotBubbleHeatmapUI("bubbleHeatmap"),
        ### Tab: change dataset
        aboutUI(req, "about"),
        ### Tab: Login form
        #tabLogin(),
        loginUI(loginNavbarTitle)
      ))
  }
  ## security_login
  ui <- secureUI(ui0)

  ### Start server code
  server <- function(input, output, session) {
    ### resize the max upload file size for admin
    options(shiny.maxRequestSize=1*1024^3) # 1G
    ### For all tags and Server-side selectize
    observe_helpers()
    optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }"
    dataSource <- reactiveValues(
      available_datasets=datasets,
      dataset=defaultDataset,
      appconf=appconf,
      data_types=data_types,
      sc1conf=NULL,
      sc1def=NULL,
      sc1gene=NULL,
      sc1meta=NULL,
      Logged=FALSE,
      terms=terms[["scRNAseq"]],
      auth=NULL,
      Username="",
      Password="",
      token="")
    ## login
    dataSource$auth <- loginServer(input, output, session)
    ## manager
    uploadServer("upload", datafolder)
    editServer("editdata", datafolder)
    aboutServer("about", reactive({dataSource}),
                optCrt, input$availableDatasets,
                datafolder)
    ## parse query strings
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if(!is.null(query[['data']])){
        updateSelectInput(session, "availableDatasets", selected=query[['data']])
      }
      if(!is.null(query[['token']])){
        token <- getToken(datafolder)
        if(query[["token"]] %in% names(token)){
          dataSource$token <- query[["token"]]
          if(dataSource$token %in% names(token)){
            updateSelectInput(session,
                              "availableDatasets",
                              selected=token[[query[['token']]]])
          }
        }
      }
    })
    ## change dataset
    observeEvent(input$availableDatasets,{
      if(!all(dataSource$available_datasets %in%
              getDataSets(datafolder = datafolder))){
        dataSource$available_datasets <- getDataSets(datafolder = datafolder)
      }
      if(!all(getDataSets(datafolder = datafolder) %in%
              dataSource$available_datasets)){
        updateSelectInput(session, "availableDatasets",
                          choices =
                            getDataSets(datafolder = datafolder,
                                        appconf =
                                          getAppConf(datafolder = datafolder)),
                          selected = input$availableDatasets)
      }
      if(input$availableDatasets %in% getDataSets(datafolder = datafolder)){
        dataSource$dataset <- input$availableDatasets
        if(checkLockedDataset(dataSource$dataset, datafolder, lockfilename="LOCKER")){
          dataSource$Logged <- FALSE
          if(dataSource$token!=""){
            if(checkToken(token, dataSource$token, dataSource$dataset)){
              dataSource$Logged <- TRUE
            }
          }else{
            if(!is.null(dataSource$auth)){
              if(checkPrivilege(dataSource$auth$privilege, dataSource$dataset)){
                dataSource$Logged <- TRUE
              }
            }
          }
          if(!dataSource$Logged){
            updateTabsetPanel(session, "topnav", selected = loginNavbarTitle)
          }
        }
        refreshData(input, output, session)
      }else{
        updateSelectInput(session, "availableDatasets",
                          choices = getDataSets(datafolder = datafolder),
                          selected = getDataSets(datafolder = datafolder)[1])
      }
    })
    ## update visitor stats
    update_visitor <- function(){
      req(input$remote_addr)
      counter <- read.delim("www/counter.tsv", header = TRUE)
      ips <- counter$ip
      counter <- as.Date(counter$date)
      visitors <- paste(format(counter, "%d/%m/%y %H"), ips)
      current <- Sys.time()
      ip <- isolate(input$remote_addr)
      agent <- isolate(input$remote_agent)
      if(!paste(format(current, "%d/%m/%y %H"), ip) %in% visitors){
        write(paste(as.character(current), ip, agent, sep="\t"),
              "www/counter.tsv", append = TRUE)
      }
    }
    observeEvent(input$remote_addr, update_visitor())
    output$total_visitor <- renderPlot({
      counter <- read.delim("www/counter.tsv", header = TRUE)
      counter <- as.Date(counter$date)
      counter <- table(format(counter, "%m/%y"))
      counter <- as.data.frame(counter)
      ggplot(counter, aes(x=Var1, y=Freq)) +
        geom_bar(stat = "identity", fill="darkorchid4") +
        theme_minimal() + xlab("") + ylab("visitor counts")
    })
    ## refresh data when change dataset
    refreshData <- function(input, output, session){
      if(dataSource$dataset %in% names(data_types)){
        dataSource$terms <- terms[[data_types[[dataSource$dataset]]]]
      }
      dataSource <- loadData(dataSource, datafolder)
      output$dataTitle <- renderUI({
        HTML(names(datasets)[datasets==input$availableDatasets])
      })

      ### Plots for tab cell info vs gene expression
      cellInfoGeneExprServer("cellInfoGeneExpr", reactive({dataSource}),
                             optCrt, input$availableDatasets,
                             datafolder)

      ### Plots for tab cell info vs cell info
      cellInfoCellInfoServer("cellInfoCellInfo", reactive({dataSource}),
                             optCrt, input$availableDatasets,
                             datafolder)

      ### Plots for tab gene expression vs gene expression
      geneExprGeneExprServer("geneExprGeneExpr", reactive({dataSource}),
                             optCrt, input$availableDatasets,
                             datafolder)

      ### Plots for tab co-expression
      coExprServer("coExpr", reactive({dataSource}),
                   optCrt, input$availableDatasets,
                   datafolder)

      ### Plots for tab subset
      subsetGeneExprServer("subsetGeneExpr", reactive({dataSource}),
                           optCrt, input$availableDatasets,
                           datafolder)

      ### Plots for tab violion
      plotVioBoxServer("vioBoxPlot", reactive({dataSource}),
                       optCrt, input$availableDatasets, datafolder)


      ### Plots for tab proportion
      plotProportionServer("proportion", reactive({dataSource}),
                           optCrt, input$availableDatasets,
                           datafolder)


      ### Plots for tab bubble heatmap
      plotBubbleHeatmapServer("bubbleHeatmap", reactive({dataSource}),
                              optCrt, input$availableDatasets,
                              datafolder)
    }
  }

  shinyApp(ui=ui, server = server, ...)
}

