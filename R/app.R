#' scRNAseqApp main function
#' @description create a scRNAseqApp once the initialization is done.
#' @param datafolder the folder where saved the dataset for the app
#' @param defaultDataset default dataset for the app.
#' @param windowTitle The title that should be displayed by the browser window.
#' @param banner The banner image.
#' @param maxRequestSize Maximal upload file size. Default is 1G.
#' @param timeout Timeout session (minutes) before logout if sleeping.
#'  Default to 30. 0 to disable.
#' @param theme A theme.
#' @param use_bs_themer logical(1). Used to determine the theme.
#' @param ... parameters can be passed to shinyApp except ui and server.
#' @import shiny
#' @importFrom utils packageVersion read.delim
#' @importFrom xfun base64_uri
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
        datafolder = "data",
        defaultDataset = "pbmc_small",
        windowTitle = "scRNAseq/scATACseq database",
        banner = system.file(
            'assets', 'img', 'banner.png',
            package = 'scRNAseqApp'),
        maxRequestSize = 1073741824,
        timeout = 30,
        theme = bs_theme(bootswatch = 'lumen'),
        use_bs_themer = FALSE,
        ...) {
    stopifnot(is(theme, "bs_theme"))
    .globals$theme <- theme
    .globals$datafolder <- datafolder
    stopifnot(file.exists(datafolder))
    ## load banner
    banner <- base64_uri(banner)
    ## load default parameters
    loginNavbarTitle <- "Switch User"
    datasets <- getDataSets()
    defaultDataset <- getDefaultDataset(defaultDataset = defaultDataset)
    appconf <- getAppConf()
    datasets <- getDataSets(appconf = appconf)
    data_types <- getDataType(appconf = appconf)
    
    ui0 <- function(req) {
        fluidPage(
            ### theme
            theme = .globals$theme,
            navbarPage(
                title = NULL,
                windowTitle = windowTitle,
                id = "topnav",
                footer = div(
                    p(
                        em(
                            windowTitle,
                            " (Version:",
                            as.character(packageVersion("scRNAseqApp")),
                            ")"
                        ),
                        HTML("&copy;"),
                        "2020 -",
                        format(Sys.Date(), "%Y"),
                        "jianhong@duke",
                        style = 'text-align:right;'
                    ),
                    class = "about-right border-top-info"
                ),
                ### Tab: change dataset
                aboutUI(req, "about", banner, defaultDataset),
                homeUI(),
                ## fake home
                navbarMenu(
                    "CellInfo/GeneExpr",
                    ### Tab: cellInfo vs geneExpr on dimRed
                    cellInfoGeneExprUI("cellInfoGeneExpr"),
                    ### Tab: cellInfo vs cellInfo on dimRed
                    cellInfoCellInfoUI("cellInfoCellInfo"),
                    ### Tab: subset gene expr
                    subsetGeneExprUI("subsetGeneExpr")
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
        gn2sym <- readRDS(
            system.file('extdata', 'gn2sym.rds', package = 'scRNAseqApp'))
        dataSource <- reactiveValues(
            available_datasets = datasets,
            # all available datasets
            dataset = defaultDataset,
            # current dataset
            appconf = appconf,
            # all data configs, used for search
            data_types = data_types,
            # data type
            cell = NULL,
            genelist = NULL,
            # genelist for cellInfoGeneExpr or heatmap plot from query string
            sc1conf = NULL,
            # config for current sc data
            sc1def = NULL,
            # def for current sc data
            sc1gene = NULL,
            # gene for current sc data
            sc1meta = NULL,
            # meta for current sc data
            search_results = NULL,
            #search_cache
            Logged = FALSE,
            # user logged
            terms = .globals$terms[["scRNAseq"]],
            # tab UI for scRNAseq/scATACseq
            symbolDict = NULL,
            # gene symbol dictionary
            gn2sym = gn2sym,
            # gene name to gene symbol dictionary
            auth = NULL,
            # authority
            Username = "",
            # username
            Password = "",
            # passward
            token = ""
        ) # toekn
        
        ### check available data
        checkAvailableDatasets <- reactivePoll(
            1000,session,
            checkFunc = getNamedDataSets,
            valueFunc = getNamedDataSets)
        observeEvent(
            checkAvailableDatasets(),
            ignoreNULL = TRUE,
            ignoreInit = TRUE,
            {
                ## update datasets if datasets changed by admin
                ad <- getNamedDataSets()
                if (!all(
                    dataSource$available_datasets %in% ad,
                    na.rm = TRUE) ||
                    !all(
                        ad %in% dataSource$available_datasets,
                        na.rm = TRUE)) {
                    dataSource$available_datasets <- ad
                    dataSource$symbolDict <-
                        updateSymbolDict()
                    updateSelectInput(
                        session,
                        "selectedDatasets",
                        choices = ad,
                        selected = input$selectedDatasets
                    )
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
        updateVisitor(input, output, session)
        ## parse query strings, and lood old session
        observe({
            query <- parseQueryString(session$clientData$url_search)
            query_has_results <- FALSE
            if (!is.null(query[['gene']])) {
                genes <- strsplit(query[['gene']], ";")[[1]]
                dataSource$genelist <- genes
            } else{
                dataSource$genelist <- NULL
            }
            if (!is.null(query[['cell']])) {
                ## not used yet
                cell <- strsplit(query[['cell']], ";")[[1]]
                dataSource$cell <- cell
            } else{
                dataSource$cell <- NULL
            }
            if (!is.null(query[['data']])) {
                updateSelectInput(
                    session, "selectedDatasets",
                    selected = query[['data']])
                session$userData[["defaultdata_init"]] <- TRUE
                query_has_results <- TRUE
            } else{
                if (!is.null(query[['token']])) {
                    token <- getToken()
                    if (query[["token"]] %in% names(token)) {
                        dataSource$token <- query[["token"]]
                        if (dataSource$token %in% names(token)) {
                            updateSelectInput(
                                session,
                                "selectedDatasets",
                                selected = 
                                    token[[query[['token']]]])
                            session$userData[["defaultdata_init"]] <- TRUE
                            query_has_results <- TRUE
                        }
                    }
                }
            }
            
            if (!use_bs_themer &
                !query_has_results) {
                ## not work when load themer
                observeEvent(input$default_defaultDataset, {
                    if (isTRUE(isolate(
                        session$userData[["defaultdata_init"]]))) {
                            return()
                    } else {
                        session$userData[["defaultdata_init"]] <- TRUE
                        dd <- isolate(input$default_defaultDataset)
                        if (!is.null(dd)) {
                            if (dd %in% getDataSets() &&
                                dd != defaultDataset) {
                                updateSelectInput(
                                    session,
                                    'selectedDatasets',
                                    selected = dd)
                            }
                        }
                    }
                })
            }
        })
        ## update gene symbol list
        dataSource$symbolDict <- updateSymbolDict()
        ## change dataset
        observeEvent(input$selectedDatasets, {
            if (input$selectedDatasets %in% getDataSets()) {
                dataSource$dataset <- input$selectedDatasets
                if (checkLocker(dataSource$dataset)) {
                    dataSource$Logged <- FALSE
                    if (dataSource$token != "") {
                        if (checkToken(
                            token,
                            dataSource$token,
                            dataSource$dataset)) {
                            dataSource$Logged <- TRUE
                        }
                    } else{
                        if (!is.null(dataSource$auth)) {
                            if (checkPrivilege(
                                dataSource$auth$privilege,
                                dataSource$dataset)) {
                                dataSource$Logged <- TRUE
                            }
                        }
                    }
                    if (!dataSource$Logged) {
                        updateTabsetPanel(
                            session,
                            "topnav",
                            selected = loginNavbarTitle)
                    }
                }
                refreshData(input, output, session)
                if (length(dataSource$genelist) == 1) {
                    updateTabsetPanel(
                        session,
                        'topnav',
                        selected = "cellInfoGeneExpr")
                } else{
                    if (length(dataSource$genelist) > 1) {
                        updateTabsetPanel(
                            session,
                            'topnav',
                            selected = "bubbleHeatmap")
                    }
                }
            } else{
                updateSelectInput(
                    session,
                    "selectedDatasets",
                    choices = getDataSets(),
                    selected = getDataSets()[1]
                )
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
            if (dataSource$dataset %in% names(data_types)) {
                dataSource$terms <-
                    .globals$terms[[data_types[[dataSource$dataset]]]]
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
        
    }
    
    shinyApp(ui = ui, server = server, ...)
}
