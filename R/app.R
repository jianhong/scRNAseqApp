#' scRNAseqApp main function
#' @description create a scRNAseqApp once the initialization is done.
#' @param app_path path, a directory where do you want to create the app
#' @param datafolder the folder where saved the dataset for the app
#' @param defaultDataset default dataset for the app.
#' @param windowTitle The title that should be displayed by the browser window.
#' @param favicon The favicon for the page.
#' @param banner The banner image.
#' @param footer The footer html contents.
#' @param maxRequestSize Maximal upload file size. Default is 1G.
#' @param timeout Timeout session (minutes) before logout if sleeping.
#'  Default to 30. 0 to disable.
#' @param theme A theme.
#' @param use_bs_themer logical(1). Used to determine the theme.
#' @param showHelpVideo logical(1) or character(1). 
#' Show help videos in homepage or not. If an url is provided, the
#' url will be embedded as a iframe element.
#' @param ... parameters can be passed to shinyApp except ui and server.
#' @import shiny
#' @importFrom utils packageVersion packageDescription read.delim
#' @importFrom xfun base64_uri is_abs_path
#' @importFrom shinyhelper observe_helpers
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal xlab ylab
#' @importFrom bslib bs_theme bs_themer
#' @export
#' @return An object that represents the app.
#' @examples
#' if(interactive()){
#'     app_path=tempdir()
#'     scInit(app_path=app_path)
#'     setwd(app_path)
#'     scRNAseqApp()
#' }

scRNAseqApp <- function(
        app_path = getwd(),
        datafolder = "data",
        defaultDataset = "pbmc_small",
        windowTitle = "scRNAseq/scATACseq database",
        favicon = system.file(
            'assets', 'img', 'favicon.ico',
            package = 'scRNAseqApp'),
        banner = system.file(
            'assets', 'img', 'banner.png',
            package = 'scRNAseqApp'),
        footer = tagList(
            HTML("&copy;"),
            "2024 -",
            format(Sys.Date(), "%Y"),
            "jianhong@morgridge"
        ),
        maxRequestSize = 1073741824,
        timeout = 30,
        theme = bs_theme(bootswatch = 'lumen'),
        use_bs_themer = FALSE,
        showHelpVideo = FALSE,
        ...) {
    stopifnot(is(theme, "bs_theme"))
    .globals$theme <- theme
    .globals$app_path <- app_path
    .globals$counterFilename <- file.path(
        app_path, "www/counter.tsv")
    if(!is_abs_path(datafolder)){
        .globals$datafolder <- file.path(app_path, datafolder)
    }else{
        .globals$datafolder <- datafolder
    }
    stopifnot(file.exists(.globals$datafolder))
    # check if the db is encrypted
    if(isEncrypted()){
        .globals$passphrase <-
            readline('Please input the passphrase for the user database:')
    }
    ## load banner
    banner <- base64_uri(banner)
    ## load default parameters
    loginNavbarTitle <- "Switch User"
    if(!tableExists(.globals$configTableName)){
        updateConfigTable()
    }
    if(!tableExists(.globals$geneSymbolTableName)){
        touchGeneTable()
    }
    datasets <- listDatasets(privilege='all', named=TRUE)
    avdb <- sort(checkAvailableDataSets(privilege = 'all'))
    if(!identical(avdb, sort(unname(datasets)))){
        updateConfigTable()
        touchGeneTable(updateDB=TRUE)
    }
    datasets <- listDatasets(named=TRUE)
    defaultDataset <- getDefaultDataset(
        defaultDataset = defaultDataset,
        datasets = datasets)
    appconf <- getAppConf(datasets = datasets)
    data_types <- getDataType(appconf = appconf)

    ui0 <- function(req) {
        fluidPage(
            ### theme
            theme = .globals$theme,
            navbarPage(
                title = tags$head(
                    tags$link(rel="icon", 
                              href=base64_uri(favicon), 
                              type="image/x-icon")),
                windowTitle = windowTitle,
                id = "topnav",
                footer = div(
                    p(
                        em(
                            windowTitle,
                            " (Version:",
                            a(as.character(packageVersion("scRNAseqApp")),
                              href = packageDescription("scRNAseqApp",
                                                        fields='URL'),
                              target = "_blank"),
                            ")"
                        ),
                        footer,
                        style = 'text-align:right;'
                    ),
                    class = "about-right border-top-info"
                ),
                ### Tab: change dataset
                aboutUI(
                    req,
                    "about",
                    banner,
                    defaultDataset,
                    datasets,
                    appconf,
                    doc = file.path(.globals$app_path, "doc.txt"),
                    showHelpVideo = showHelpVideo),
                ## fake home
                homeUI(),
                navbarMenu(
                    "CellInfo/GeneExpr",
                    ### Tab: cellInfo vs geneExpr on dimRed
                    cellInfoGeneExprUI("cellInfoGeneExpr"),
                    ### Tab: cellInfo vs cellInfo on dimRed
                    cellInfoCellInfoUI("cellInfoCellInfo"),
                    ### Tab: subset gene expr
                    subsetGeneExprUI("subsetGeneExpr"),
                    ### Tab: ATAC vs Gene expr
                    geneAccGeneExprUI("ATACvsExpr")
                ),
                navbarMenu(
                    "Co-expression",
                    ### Tab: geneExpr vs geneExpr on dimRed
                    geneExprGeneExprUI("geneExprGeneExpr"),
                    ### Tab: Gene coexpression plot
                    coExprUI("coExpr"),
                    ### Tab: 3d Gene coexpression plot
                    coExpr3dUI("coExpr3d"),
                    ### Tab: sunburst plot
                    plotPieDimUI('sunburst')
                ),
                navbarMenu(
                    "Stats",
                    ### Tab: violinplot / boxplot
                    plotVioBoxUI("vioBoxPlot"),
                    ### Tab: Proportion plot
                    plotProportionUI("proportion"),
                    ### Tab: Multiple gene expr
                    plotBubbleHeatmapUI("bubbleHeatmap"),
                    ### Tab: monocle
                    #plotMonocleUI("monocle"),
                    ### Tab: waffle
                    plotWaffleUI("waffle")
                ),
                subsetPlotsUI('explorer'),
                ### Tab: downloader
                downloaderUI('downloader'),
                ### Tab: comments
                issueUI('issues'),
                ### Tab: Login form
                #tabLogin(),
                loginUI(loginNavbarTitle, defaultDataset)
            )
        )
    }
    ## security_login
    ui <- secureUI(ui0, timeout)
    
    ### Start server code
    server <- function(input, output, session) {
        ## load local storage
        session$sendCustomMessage("load_key", "defaultDataset")
        ## set theme
        if (is.null(getShinyOption("bootstrapTheme"))) {
            shinyOptions("bootstrapTheme" = .globals$theme)
        }
        if (use_bs_themer &&
            is(getShinyOption("bootstrapTheme"), "bs_theme")) {
            bs_themer()
        }
        ### resize the max upload file size for admin
        options(shiny.maxRequestSize = maxRequestSize) # 1G
        ### For all tags and Server-side selectize
        observe_helpers()
        optCrt <- "{ option_create: function(data,escape) {
    return('<div class=\"create\"><strong>' + '</strong></div>');
    } }"
        touchGenename2Symbol()
        dataSource <- reactiveValues(
            # all available datasets
            available_datasets = datasets,
            # current dataset
            dataset = defaultDataset,
            # all data configs, used for search
            appconf = appconf,
            # data type
            data_types = data_types,
            cell = NULL,
            # genelist for cellInfoGeneExpr or heatmap plot from query string
            genelist = NULL,
            # active tab from query string
            activetab = NULL,
            # config for current sc data
            sc1conf = NULL,
            # def for current sc data
            sc1def = NULL,
            # gene for current sc data
            sc1gene = NULL,
            # meta for current sc data
            sc1meta = NULL,
            #search_cache
            search_results = NULL,
            # user logged
            Logged = FALSE,
            # tab UI for scRNAseq/scATACseq
            terms = .globals$terms[["scRNAseq"]],
            # authority
            auth = NULL,
            # username
            Username = "",
            # passward
            Password = "",
            # toekn
            token = ""
        ) 
        
        updateDataSource <- function(datasets, selected){
            if(missing(datasets)) datasets <- 
                    listDatasets(privilege = dataSource$auth$privilege,
                                 named=TRUE)
            appconf <- getAppConf(
                datasets = datasets,
                privilege = 'all'
            )
            if(any(!datasets %in% names(appconf))){
                warning('Some dataset does not contain appconf!')
            }
            dataSource$appconf <- appconf
            dataSource$available_datasets <- datasets
            dataSource$dataset <- selected
            dataSource$data_types <- getDataType(appconf = appconf)
            updateSelectInput(
                session,
                "selectedDatasets",
                choices = dataSource$available_datasets,
                selected = dataSource$dataset
            )
        }
        
        ### check available data
        compareAvailableDatasets <- reactivePoll(
            1000,session,
            checkFunc = function(){
                sort(checkAvailableDataSets(
                    privilege = dataSource$auth$privilege))
                },
            valueFunc = function(){
                sort(unname(isolate(dataSource$available_datasets)))
            })
        observeEvent(
            compareAvailableDatasets(),
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            {
                updateConfigTable()
                touchGeneTable(updateDB=TRUE)
                ad <- listDatasets(privilege = dataSource$auth$privilege,
                                   named=TRUE)
                if (!all(
                    dataSource$available_datasets %in% ad,
                    na.rm = TRUE) ||
                    !all(
                        ad %in% dataSource$available_datasets,
                        na.rm = TRUE)) {
                    updateDataSource(ad, input$selectedDatasets)
                }
            })
        ## login
        dataSource$auth <- loginServer(input, output, session)
        observe({
            if (!is.null(isolate(dataSource$auth$admin))) {
                if (isolate(dataSource$auth$admin)) {
                    webstatsServer("webstats")
                    uploadServer("upload")
                    editServer("editdata")
                }
            }
        })
        ## update visitor stats
        updateVisitorTable(input, output, session)
        ## parse query strings, and load old session
        observe({
            query <- parseQueryString(session$clientData$url_search)
            query_has_results <- FALSE
            if (!is.null(query[['gene']])) {
                genes <- strsplit(query[['gene']], ";")[[1]]
                dataSource$genelist <- genes
            } else{
                dataSource$genelist <- NULL
            }
            if (!is.null(query[['tab']])) {
                dataSource$activetab <- query[['tab']]
            } else {
                dataSource$activetab <- NULL
            }
            if (!is.null(query[['cell']])) {
                ## not used yet
                cell <- strsplit(query[['cell']], ";")[[1]]
                dataSource$cell <- cell
            } else{
                dataSource$cell <- NULL
            }
            p_query <- parseQuery(query, defaultDataset)
            if(p_query['from']=='data'){
                session$userData[["defaultdata_init"]] <- TRUE
                query_has_results <- TRUE
                if(p_query["defaultDataset"] %in% dataSource$available_datasets){
                    defaultDataset <- p_query["defaultDataset"]
                    updateSelectInput(
                        session,
                        'selectedDatasets',
                        choices = dataSource$available_datasets,
                        selected = p_query["defaultDataset"]
                    )
                }
            }else{
                if(p_query['from']=='token'){
                    dataSource$token <- query[["token"]]
                    defaultDataset <- p_query["defaultDataset"]
                    session$userData[["defaultdata_init"]] <- TRUE
                    query_has_results <- TRUE
                    showNotification(
                        "Datasets from token!",
                        duration = 3,
                        type = "message")
                    updateSelectInput(
                        session,
                        'selectedDatasets',
                        choices = c(dataSource$available_datasets,
                                    p_query["defaultDataset"]),
                        selected = p_query["defaultDataset"])
                }
            }
            
            if (!use_bs_themer &
                !query_has_results) {
                ## not work when load themer
                ## load remote user data
                observeEvent(input$default_defaultDataset, {
                    if (isTRUE(isolate(
                        session$userData[["defaultdata_init"]]))) {
                            return()
                    } else {
                        session$userData[["defaultdata_init"]] <- TRUE
                        dd <- isolate(input$default_defaultDataset)
                        if (!is.null(dd)) {
                            if (dd %in% dataSource$available_datasets &&
                                dd != dataSource$dataset) {
                                showNotification(
                                    "Resuming your last session!",
                                    duration = 3,
                                    type = "message")
                                updateSelectInput(
                                    session,
                                    'selectedDatasets',
                                    choices = dataSource$available_datasets,
                                    selected = dd)
                            }
                        }
                    }
                })
            }
        })
        ## when click the search results
        observeEvent(input$search_results_select_button, {
            updateSelectInput(session, 'selectedDatasets',
                              selected=input$search_results_select_button)
        })
        ## change dataset
        observeEvent(input$selectedDatasets, {
            ## in case the data is deleted, refresh the datasets
            availabledb <- 
                checkAvailableDataSets(privilege = dataSource$auth$privilege,
                                       token = dataSource$token)
            if (input$selectedDatasets %in% availabledb) {
                # in available datasets for current user
                if(dataSource$token != ""){#with token
                    if (checkToken(
                        getTokenList(),
                        dataSource$token,
                        input$selectedDatasets)) {
                        dataSource$Logged <- TRUE
                        dataSource$auth$privilege <- 
                            unique(c(dataSource$auth$privilege,
                                     input$selectedDatasets))
                        res <- updateDatasetForToken(
                            input$selectedDatasets,
                            dataSource$available_datasets)
                        dataSource$available_datasets <- res$datasets
                        dataSource$appconf <- res$appconf
                        dataSource$data_types <- res$data_types
                        dataSource$dataset <- input$selectedDatasets
                    }
                    updateDataSource(
                        datasets = listDatasets(availabledb,
                                                privilege = 'all',
                                                named = TRUE),
                        selected = input$selectedDatasets)

                }
                redirectionData()
            } else{
                if (checkLocker(input$selectedDatasets)) {
                    # dataset is not open to current user
                    dataSource$Logged <- FALSE
                    if (dataSource$token != "") {} else{
                        if (!is.null(dataSource$auth)) {
                            if (checkPrivilege(
                                dataSource$auth$privilege,
                                input$selectedDatasets)) {
                                dataSource$Logged <- TRUE
                                dataSource$available_datasets <- 
                                    listDatasets(dataSource$auth$privilege,
                                                 named=TRUE)
                                updateDataSource(
                                    datasets = dataSource$available_datasets,
                                    selected = input$selectedDatasets)
                            }
                        }
                    }
                    if (!dataSource$Logged) {
                        updateTabsetPanel(
                            session,
                            "topnav",
                            selected = loginNavbarTitle)
                    }else{
                        redirectionData()
                    }
                }else{
                    ##data was deleted
                    updateDataSource(datasets = dataSource$available_datasets,
                                     selected = availabledb[1])
                }
            }
            session$sendCustomMessage(
                "save_key",
                paste(
                    "defaultDataset",
                    isolate(input$selectedDatasets),
                    sep = "|"
                ))
        })
        ## refresh data when change dataset
        refreshData <- function(input, output, session) {
            if (dataSource$dataset %in% names(dataSource$data_types)) {
                dataSource$terms <-
                    .globals$terms[[
                        dataSource$data_types[[
                            dataSource$dataset]]]]
            }
            dataSource <- loadData(dataSource)
            output$dataTitle <- renderUI({
                HTML(names(datasets)[datasets == input$selectedDatasets])
            })
            
            aboutServer("about", reactive({
                dataSource
            }), optCrt)
            ### Plots for tab cell info vs gene expression
            cellInfoGeneExprServer(
                "cellInfoGeneExpr",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for tab cell info vs cell info
            cellInfoCellInfoServer(
                "cellInfoCellInfo",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for tab gene expression vs gene expression
            geneExprGeneExprServer(
                "geneExprGeneExpr",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### ATAC vs Expr
            geneAccGeneExprServer(
                "ATACvsExpr",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for tab co-expression
            coExprServer(
                "coExpr",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for tab 3d co-expression
            coExpr3dServer(
                "coExpr3d",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for tab PieDim
            plotPieDimServer(
                'sunburst',
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for tab subset
            subsetGeneExprServer(
                "subsetGeneExpr",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for tab violion
            plotVioBoxServer(
                "vioBoxPlot",
                reactive({
                    dataSource
                }),
                optCrt)
            
            
            ### Plots for tab proportion
            plotProportionServer(
                "proportion",
                reactive({
                    dataSource
                }),
                optCrt)
            
            
            ### Plots for tab bubble heatmap
            plotBubbleHeatmapServer(
                "bubbleHeatmap",
                reactive({
                    dataSource
                }),
                optCrt)
            
            ### Plots for waffle
            plotWaffleServer(
                "waffle",
                reactive({
                    dataSource
                }),
                optCrt)
            
            subsetPlotsServer(
                'explorer',
                reactive({
                    dataSource
                }),
                optCrt)
        }
        ## update UI and server
        redirectionData <- function(){
            dataSource$dataset <- input$selectedDatasets
            refreshData(input, output, session)
            if (length(dataSource$genelist) == 1) {
                updateTabsetPanel(
                    session,
                    'topnav',
                    selected = ifelse(is.null(dataSource$activetab),
                                      "cellInfoGeneExpr",
                                      dataSource$activetab))
            } else{
                if (length(dataSource$genelist) > 1) {
                    updateTabsetPanel(
                        session,
                        'topnav',
                        selected = "bubbleHeatmap")
                }
            }
        }
        ## comments server
        issueServer('issues',
                    reactive({
                        dataSource
                    }))
        ## download server
        downloaderServer("downloader")
        ## observe event for editor
        reLoadData <- function(){
            dataSource <- loadData(dataSource)
        }
        observeEvent(input$editorStatus, {
            reLoadData()
        })
    }
    shinyApp(ui = ui, server = server, ...)
}
