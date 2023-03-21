homeUI <- function() {
    tabPanel(## fack home
        title = "",
        value = "home",
        icon = icon("home"))
}

#' @importFrom bibtex read.bib
#' @importFrom RefManageR GetBibEntryWithDOI PrintBibliography
aboutUI <- function(
    request, id, banner, defaultDataset, datasets, appconf, doc = "doc.txt") {
        ns <- NS(id)
        query <- parseQueryString(request[["QUERY_STRING"]])
        defaultDataset <- 
            parseQuery(query, defaultDataset)[1]
        tabPanel(
            title = div(
                selectInput(
                    'selectedDatasets',
                    label = NULL,
                    choices = getNamedDataSets(
                        datasets=datasets,
                        appconf=appconf),
                    selected = defaultDataset,
                    width = "90vw"
                )
            ),
            value = 'about',
            div(id = 'about-doc', includeHTML(doc)),
            hr(),
            htmlOutput(ns("ref")),
            hr(),
            div(
                class = "about-display-container about-content about-top",
                style = 'width:100%;min-height:300px;',
                img(class = "about-image about-glass", src = banner),
                absolutePanel(
                    top = "15%",
                    left = "10%",
                    width = "80%",
                    draggable = TRUE,
                    div(
                        class =
                            "panel panel-danger about-glass",
                        div(
                            class =
                                paste(
                                    "panel-heading",
                                    "about-container",
                                    "about-padding-16"),
                            textInput(
                                ns('search'),
                                label = NULL,
                                width = "96%",
                                placeholder = 'Search the database'
                            )
                        ),
                        div(
                            class = paste(
                                "panel-body",
                                "about-bar",
                                "border-bottom-info",
                                "shadow h-100 py-2"),
                            htmlOutput(ns('search_res')))
                    )
                )
            ),
            hr(),
            div(
                class = "about-container",
                summaryBox(
                    "datasets",
                    textOutput(ns('dataset_counts')),
                    width = 3,
                    icon = "database",
                    style = "success",
                    border = "bottom"
                ),
                summaryBox(
                    "references",
                    textOutput(ns('reference_count')),
                    width = 3,
                    icon = "book-open",
                    style = "danger",
                    border = "bottom"
                ),
                summaryBox(
                    "visitors",
                    textOutput(ns('visitor_count')),
                    width = 3,
                    icon = "eye",
                    style = "primary",
                    border = "bottom"
                ),
                summaryBox(
                    "species",
                    textOutput(ns('species_count')),
                    width = 3,
                    icon = "fish",
                    style = "warning",
                    border = "bottom"
                )
            ),
            hr(),
            h4("Full reference list:"),
            get_full_ref_list(appconf),
            p(
                imageOutput('total_visitor', width = "50%", height = "150px")
            ),
            div(
                style = "display:none;",
                textInput(
                    "remote_agent", "remote_agent",
                    value = request[["HTTP_USER_AGENT"]]),
                visitorDependencies()
            ),
            hr(),
            p(
                em("This webpage was modified from "),
                a(
                    "ShinyCell",
                    href = "https://github.com/SGDDNB/ShinyCell",
                    target = "_blank")
            )
        )
    }
aboutServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        bibentry <- getRef(
            dataSource()$dataset,
            "entry",
            dataSource()$appconf)
        global <- reactiveValues(search_results = list(), evt = list())
        if (is(bibentry, "bibentry")) {
            output$ref <- renderUI(tagList(if (!is.null(bibentry$abstract)) {
                if (!grepl(
                    "^The abstract of a scientific paper represents a concise",
                    bibentry$abstract
                )) {
                    tagList(h5("Details for current data"),
                            HTML(format(
                                bibentry$abstract, style = "html"
                            )))
                } else{
                    h5("Reference for current data")
                }
            } else{
                h5("Reference for current data")
            },
            HTML(
                format(bibentry, style = "html")
            )))
        } else{
            ref <- getRef(
                dataSource()$dataset,
                "bib",
                dataSource()$appconf)
            if (!is.null(ref) && !is.na(ref)) {
                output$ref <- renderUI(tagList(h5(
                    "Reference for current data"
                ),
                HTML(ref)))
            }
        }
        observeEvent(input$search, {
            if (input$search != '' && input$search != "Type key words here") {
                output$search_res <- renderUI(tags$div('searching...'))
                updateSearch(
                    input$search,
                    dataSource()$symbolDict,
                    dataSource()$gn2sym,
                    auth = dataSource()$auth,
                    global = reactive({
                        global
                    }),
                    id = id,
                    input = input,
                    output = output,
                    session = session
                )
            }
        })
        output$dataset_counts <- renderText({
            length(dataSource()$available_datasets)
        })
        output$visitor_count <- renderText({
            counter <- read.delim(
                .globals$counterFilename,
                header = TRUE)
            length(unique(counter$ip))
        })
        output$reference_count <- renderText({
            get_full_ref_list(dataSource()$appconf, returnLen = TRUE)
        })
        output$species_count <- renderText({
            length(unique(lapply(dataSource()$appconf, `[[`, i = "species")))
        })
    })
}
