homeUI <- function() {
    tabPanel(## fack home
        title = "",
        value = "home",
        icon = icon("home"))
}

#' @importFrom bibtex read.bib
#' @importFrom RefManageR GetBibEntryWithDOI PrintBibliography
aboutUI <- function(
    request, id, banner, darkbanner, defaultDataset, datasets, appconf, doc = "doc.txt",
    showHelpVideo=FALSE) {
        ns <- NS(id)
        if(is.character(showHelpVideo)){
            helperVideoURL <- showHelpVideo
            showHelpVideo <- TRUE
        }else{
            helperVideoURL <- "https://www.youtube.com/embed/videoseries?si=VgcP3HxCrvtp59yS&amp;list=PL0uThf-sipbnepUDzVmAHpNWUy65F4HOm&amp;showinfo=0"
        }
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
            }else{
                if(defaultDataset %in% names(datasets)){
                   defaultDataset <- datasets[defaultDataset]
                }else{
                    ## something wrong here
                    defaultDataset <- datasets[1] 
                }
            }
        }
        refs <- listReferences()
        species <- listSpecies()
        dbs <- listDatasets(
            key = datasets,
            privilege='all',
            named = TRUE)
        tabPanel(
            title = div(
                selectInput(
                    'selectedDatasets',
                    label = NULL,
                    choices = dbs,
                    selected = defaultDataset,
                    width = "100%"
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
                div(
                    class = "banner-wrapper",
                    img(class = "about-image about-glass banner-light",
                        src = banner),
                    img(class = "about-image about-glass banner-dark",
                        src = darkbanner)
                ),
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
                            fluidRow(
                                column(9,
                                       textInput(
                                           ns('search'),
                                           label = NULL,
                                           width = "100%",
                                           placeholder = 'Search the database'
                                       )),
                                column(1, 
                                      actionButton(
                                          ns('sbtn'),
                                          label = 'Search',
                                          width = '100px'
                                      ))
                            )
                        ),
                        div(
                            class = paste(
                                "panel-body",
                                "about-bar",
                                "border-bottom-info",
                                "shadow h-100 py-2"),
                            htmlOutput(ns('search_res')),
                            div(
                                style = "visibility:hidden;",
                                checkboxInput(
                                    ns("s_res_flag"),
                                    label = NULL,
                                    value = FALSE
                                )
                            ))
                    )
                )
            ),
            hr(),
            div(
                class = "about-container",
                summaryBox(
                    "DATASETS",
                    length(dbs), #textOutput(ns('dataset_counts')),
                    width = 3,
                    icon = "database",
                    style = "success",
                    border = "bottom",
                    info = 'Public dataset list:',
                    details = paste("<ol>\n",
                                    paste(paste0("<li><p><a href='?data=",
                                                dbs,
                                                "'>",
                                                names(dbs),
                                                "</a></p></li>"), collapse = "\n"),
                                    "\n</ol>"),
                    id = id
                ),
                summaryBox(
                    "REFERENCES",
                    length(refs), #textOutput(ns('reference_count')),
                    width = 3,
                    icon = "book-open",
                    style = "danger",
                    border = "bottom",
                    info = 'Full reference list:',
                    details = paste("<ol>\n",
                                    paste(paste("<li>",
                                                refs,
                                                "</li>"), collapse = "\n"),
                                    "\n</ol>")
                ),
                summaryBox(
                    "VISITORS",
                    listVisitors(ipCounter = TRUE)$uniqueIP, #textOutput(ns('visitor_count')),
                    width = 3,
                    icon = "eye",
                    style = "primary",
                    border = "bottom",
                    info = 'Vistor counts:',
                    details = ''
                ),
                summaryBox(
                    "SPECIES",
                    tags$a(length(species),
                           'data-toggle'='tooltip',
                           'data-html'="true",
                           title=paste(species,
                                       collapse = ',\u{000A}')), #textOutput(ns('species_count')),
                    width = 3,
                    icon = "fish",
                    style = "warning",
                    border = "bottom",
                    info = 'Full species list:',
                    details = paste("<ul>\n",
                                    paste(paste0("<li><p><a href='https://www.google.com/search?q=",
                                                 species,
                                                 "' target='_blank'>",
                                                 species,
                                                 "</a></p></li>"), collapse = "\n"),
                                    "\n</ul>")
                )
            ),
            hr(),
            div(
                id = 'informationBox',
                uiOutput(ns('privateDatasets')),
                h4("Full reference list:"),
                div(id='infolist',
                    HTML(paste("<ol>\n",
                           paste(paste("<li>",
                                       refs,
                                       "</li>"), collapse = "\n"),
                           "\n</ol>")))
            ),
            if(!showHelpVideo){
                p(
                    imageOutput('total_visitor', width = "50%", height = "150px")
                )
            }else{
                fluidRow(
                    column(
                        6,
                        imageOutput('total_visitor',
                                    width = "100%", height = "240px")),
                    column(
                        6,
                        tags$iframe(
                            width="100%",
                            height='240px',
                            src=helperVideoURL,
                            frameborder="0",
                            allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share;",
                            referrerpolicy="strict-origin-when-cross-origin",
                            allowfullscreen=NA)
                    )
                )
            },
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
            ),
            div(id = "splash-screen",
                div(class = "ppt-viewport",
                    uiOutput(ns("markdownSlides"))
                )
            )
        )
}
#' @importFrom markdown markdownToHTML
aboutServer <- function(id, dataSource, optCrt) {
    moduleServer(id, function(input, output, session) {
        output$markdownSlides <- renderUI({
            session$onFlushed(function() {
                session$sendCustomMessage(type = "start_ppt",
                                          message = list(interval = 10000))
            }, once = TRUE)
            file_path <- file.path(.globals$datafolder,
                                   .globals$filenames$welcomepage)
            if(!file.exists(file_path)){
                return(div(class = "slide-item",
                           div(class = "image-slide", 
                               div(
                                   class =
                                       "panel panel-danger",
                                   div(
                                       class =
                                           paste(
                                               "panel-heading",
                                               "about-container",
                                               "about-padding-16"),
                                   h1("Welcome!"),
                                   p('Getting Everything Ready for You.',
                                     'The App Will Launch Shortly.')
                                   )
                               )
                           ),
                           div(class = "splash-footer",
                               actionButton(inputId = "close-splash",
                                            class = "close-splash-btn",
                                            label = "Close & Continue",
                                            icon = icon('close'))
                           )
                ))
            }
            # Read raw lines of markdown text
            lines <- readLines(file_path, warn = FALSE)
            # Find positions where structural headers begin (using H3 tags here)
            header_indices <- grep("^### ", lines)
            if (length(header_indices) == 0) {
                return(div(class = "slide-item",
                    h1("Welcome!"),
                    p("Find welcome.md but ", 
                    "no '###' headers found to slice into slides."),
                    div(class = "splash-footer",
                        actionButton(inputId = "close-splash",
                                     class = "close-splash-btn",
                                     label = "Close & Continue",
                                     icon = icon('close'))
                    )
                ))}
            
            slide_blocks <- list()
            num_headers <- length(header_indices)
            
            for (i in seq_len(num_headers)) {
                start_line <- header_indices[i]
                # Pull up until next header or the absolute end of the file
                end_line <- 
                    if (i < num_headers) {
                        header_indices[i + 1] - 1
                    } else length(lines)
                slide_text <- lines[start_line:end_line]
                # Process this individual slide segment chunk into standalone HTML elements
                slide_html <- HTML(markdown::markdownToHTML(
                    text = paste(slide_text, collapse = "\n"),
                    fragment.only = TRUE))
                # Wrap each into our transition-capable slide-item container
                slide_blocks[[i]] <- div(class = "slide-item", slide_html)
            }
            tagList(slide_blocks,
                    div(class = "splash-footer",
                        actionButton(inputId = "close-splash",
                                     class = "close-splash-btn",
                                     label = "Close & Continue",
                                     icon = icon('close'))
                    ))
        })
        
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
        output$statsDATASETS <- 
            renderText({length(dataSource()$available_datasets)})
        observeEvent(dataSource()$available_datasets, {
            updateSelectInput(inputId = 'selectedDatasets',
                    label = NULL,
                    choices = dataSource()$available_datasets,
                    selected = dataSource()$dataset
            )
            output$statsDATASETS <- 
                renderText({length(dataSource()$available_datasets)})
            ## private datasets
            publicDbs <- checkAvailableDataSets(privilege = NULL)
            privateDbs <- setdiff(dataSource()$available_datasets, publicDbs)
            if(length(privateDbs)){
                output$privateDatasets <- renderUI({
                    tagList(
                        h5('Private dataset list:'),
                        tags$ul(
                            lapply(privateDbs, function(.ele){
                                # add token if logged
                                token <- getLogToken(session = session)
                                if(!is.null(token)){
                                    token <- paste0('&token=', token)
                                }
                                return(tags$li(
                                    tags$a(
                                        href=paste0('?data=', .ele, token),
                                        .ele,
                                        onclick = 
                                            paste0('event.preventDefault();',
                                                   'Shiny.onInputChange(',
                                                   '\"search_results_select_button\",  \"',
                                                   .ele,'\");window.scrollTo(0, 0);'))
                                ))
                            })
                        ) 
                    )
                })
            }
        })
        observeEvent(input$sbtn, {
            if (input$search != '' && input$search != "Type key words here") {
                showNotification(
                    paste('Searching', input$search, 'in database.'),
                    duration = 3,
                    type = 'message'
                )
                updateSearch(
                    input$search,
                    dataSource()$available_datasets,
                    auth = dataSource()$auth,
                    global = reactive({
                        global
                    }),
                    id = id,
                    input = input,
                    output = output,
                    session = session
                )
            }else{
                output$search_res <- renderUI(tags$div())
            }
        })
    })
}
