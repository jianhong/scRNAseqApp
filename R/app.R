#' scRNAseqApp main function
#' @description create a scRNAseqApp once the initialization is done.
#' @param datafolder the folder where saved the dataset for the app
#' @param defaultDataset default dataset for the app.
#' @param windowTitle The title that should be displayed by the browser window.
#' @param banner The banner image.
#' @param maxRequestSize Maximal upload file size. Default is 1G.
#' @param theme A theme.
#' @param use_bs_themer logical(1). Used to determine the theme.
#' @param ... parameters can be passed to shinyApp except ui and server.
#' @import shiny
#' @importFrom utils packageVersion read.delim
#' @importFrom xfun base64_uri
#' @importFrom shinyhelper observe_helpers
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal xlab ylab
#' @importFrom bslib bs_theme bs_themer
#' @importFrom thematic thematic_shiny
#' @export
#' @return An object that represents the app.
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
                        banner = system.file('assets', 'images', 'banner.png',
                                             package='scRNAseqApp'),
                        maxRequestSize=1073741824,
                        theme = bs_theme(bootswatch = 'lumen'),
                        use_bs_themer = FALSE,
                        ...){
  stopifnot(is(theme, "bs_theme"))
  thematic_shiny(font = "auto")
  ## load banner
  banner <- base64_uri(banner)
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
      ### theme
      theme = theme,
      navbarPage(
        title = NULL,
        windowTitle = windowTitle,
        id = "topnav",
        footer = div(p(em(windowTitle, " (Version:",
                          as.character(packageVersion("scRNAseqApp")), ")"),
                       HTML("&copy;"), "2020 -",
                       format(Sys.Date(), "%Y"),
                       "jianhong@duke", style='text-align:right;'),
                     class="about-right border-top-info"),
        ### Tab: change dataset
        aboutUI(req, "about", datafolder, banner),
        homeUI(), ## fake home
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
        ### Tab: Login form
        #tabLogin(),
        loginUI(loginNavbarTitle, defaultDataset)
      ))
  }
  ## security_login
  ui <- secureUI(ui0)

  ### Start server code
  server <- function(input, output, session) {
    ## load local storage
    session$sendCustomMessage("load_key", "defaultDataset")
    ## set theme
    if(is.null(getShinyOption("bootstrapTheme"))){
      shinyOptions("bootstrapTheme"=theme)
    }
    if(use_bs_themer && is(getShinyOption("bootstrapTheme"), "bs_theme")){
      bs_themer()
    }

    ### resize the max upload file size for admin
    options(shiny.maxRequestSize=maxRequestSize) # 1G
    ### For all tags and Server-side selectize
    observe_helpers()
    optCrt="{ option_create: function(data,escape) {return('<div class=\"create\"><strong>' + '</strong></div>');} }"
    dataSource <- reactiveValues(
      available_datasets=datasets,
      dataset=defaultDataset,
      appconf=appconf,
      data_types=data_types,
      genelist=NULL,
      sc1conf=NULL,
      sc1def=NULL,
      sc1gene=NULL,
      sc1meta=NULL,
      Logged=FALSE,
      terms=terms[["scRNAseq"]],
      symbolDict=NULL,
      auth=NULL,
      Username="",
      Password="",
      token="")
    ## login
    dataSource$auth <- loginServer(input, output, session)
    ## manager
    uploadServer("upload", datafolder)
    editServer("editdata", datafolder)
    ## update visitor stats
    updateVisitor(input, output, session)
    ## parse query strings, and lood old session
    observe({
      query <- parseQueryString(session$clientData$url_search)
      query_has_results <- FALSE
      if(!is.null(query[['gene']])){
        genes <- strsplit(query[['gene']], ";")[[1]]
        dataSource$genelist <- genes
      }else{
        dataSource$genelist <- NULL
      }
      if(!is.null(query[['data']])){
        updateSelectInput(session, "availableDatasets",
                          selected=query[['data']])
        session$userData[["defaultdata_init"]] <- TRUE
        query_has_results <- TRUE
      }else{
        if(!is.null(query[['token']])){
          token <- getToken(datafolder)
          if(query[["token"]] %in% names(token)){
            dataSource$token <- query[["token"]]
            if(dataSource$token %in% names(token)){
              updateSelectInput(session,
                                "availableDatasets",
                                selected=token[[query[['token']]]])
              session$userData[["defaultdata_init"]] <- TRUE
              query_has_results <- TRUE
            }
          }
        }
      }

      if(!use_bs_themer & !query_has_results){ ## not work when load themer
        observeEvent(input$default_defaultDataset, {
          if (isTRUE(isolate(session$userData[["defaultdata_init"]]))) {
            return()
          } else {
            session$userData[["defaultdata_init"]] <- TRUE
            dd <- isolate(input$default_defaultDataset)
            if(!is.null(dd)){
              if(dd %in% getDataSets(datafolder = datafolder) &&
                 dd!=defaultDataset){
                updateSelectInput(session, 'availableDatasets', selected = dd)
              }
            }
          }
        })
      }
    })
    ## update gene symbol list
    dataSource$symbolDict <- updateSymbolDict(datafolder)
    ## change dataset
    observeEvent(input$availableDatasets,{
      ## update datasets if datasets changed by admin
      if(!all(dataSource$available_datasets %in%
              getDataSets(datafolder = datafolder))){
        dataSource$available_datasets <- getDataSets(datafolder = datafolder)
      }
      if(!all(getDataSets(datafolder = datafolder) %in%
              dataSource$available_datasets)){
        dataSource$symbolDict <- updateSymbolDict(datafolder)
        updateSelectInput(session, "availableDatasets",
                          choices =
                            getDataSets(datafolder = datafolder,
                                        appconf =
                                          getAppConf(datafolder = datafolder)),
                          selected = input$availableDatasets)
      }

      if(input$availableDatasets %in% getDataSets(datafolder = datafolder)){
        dataSource$dataset <- input$availableDatasets
        if(checkLockedDataset(dataSource$dataset, datafolder,
                              lockfilename="LOCKER")){
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
        if(length(dataSource$genelist)==1){
          updateTabsetPanel(session, 'topnav', selected ="cellInfoGeneExpr")
        }else{
          if(length(dataSource$genelist)>1){
            updateTabsetPanel(session, 'topnav', selected ="bubbleHeatmap")
          }
        }
      }else{
        updateSelectInput(session, "availableDatasets",
                          choices = getDataSets(datafolder = datafolder),
                          selected = getDataSets(datafolder = datafolder)[1])
      }
      session$sendCustomMessage("save_key",
                                paste("defaultDataset",
                                      isolate(input$availableDatasets),
                                      sep = "|"))
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

      aboutServer("about", reactive({dataSource}),
                  optCrt, input$availableDatasets,
                  datafolder)
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

