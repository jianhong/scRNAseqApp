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
        query <- parseQuery(query, defaultDataset)
        defaultDataset <- query['defaultDataset']
        if(!defaultDataset %in% datasets){
            if(query['from']=='token'){
                ## from token
                res <- updateDatasetForToken(defaultDataset, datasets)
                datasets <- res$datasets
                appconf <- res$appconf
                data_types <- res$data_types
            }else{## something wrong here
                defaultDataset <- datasets[1]
            }
        }
        refs <- listReferences()
        tabPanel(
            title = div(
                selectInput(
                    'selectedDatasets',
                    label = NULL,
                    choices = listDatasets(
                        key = datasets,
                        named = TRUE),
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
                    length(listDatasets()), #textOutput(ns('dataset_counts')),
                    width = 3,
                    icon = "database",
                    style = "success",
                    border = "bottom"
                ),
                summaryBox(
                    "references",
                    length(refs), #textOutput(ns('reference_count')),
                    width = 3,
                    icon = "book-open",
                    style = "danger",
                    border = "bottom"
                ),
                summaryBox(
                    "visitors",
                    length(unique(read.delim(
                        .globals$counterFilename,
                        header = TRUE)$ip)), #textOutput(ns('visitor_count')),
                    width = 3,
                    icon = "eye",
                    style = "primary",
                    border = "bottom"
                ),
                summaryBox(
                    "species",
                    length(listSpecies()), #textOutput(ns('species_count')),
                    width = 3,
                    icon = "fish",
                    style = "warning",
                    border = "bottom"
                )
            ),
            hr(),
            h4("Full reference list:"),
            HTML(paste("<ol>\n",
                       paste(paste("<li>",
                                   refs,
                                   "</li>"), collapse = "\n"),
                       "\n</ol>")),
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
        bibentry <- function(key){
            getRef(dataSource()$dataset, key, dataSource()$appconf)
        }
        global <- reactiveValues(search_results = list(), evt = list())
        output$ref <- renderUI(
            if(!is.na(bibentry('ref_title'))){
                tagList(
                    h5(bibentry('ref_title')),
                    if (!is.na(bibentry("ref_abstract"))) {
                        if(!grepl(
                    "^The abstract of a scientific paper represents a concise",
                            bibentry("ref_abstract")
                        )){
                            HTML(bibentry("ref_abstract"))
                        }
                    },
                    h5("Reference for current data"),
                    HTML(
                        bibentry("ref_bib")
                    )
                )
            }else{
                h5('No reference for current data.')
            })
        observeEvent(input$search, {
            invalidateLater(2000, session = session)
            if (input$search != '' && input$search != "Type key words here") {
                output$search_res <- renderUI(tags$div('searching...'))
                updateSearch(
                    input$search,
                    dataSource()$available_datasets,
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
    })
}
