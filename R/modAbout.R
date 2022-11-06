#' @importFrom htmltools htmlDependency
visitorDependencies <- function(){
  htmlDependency(name = "scRNAseqApp-assets", version = "0.0.1",
                 package = "scRNAseqApp",
                 src = "assets",
                 script = "js/login.js",
                 stylesheet = c("css/style.css")
  )
}
#' @importFrom bibtex read.bib
#' @importFrom RefManageR GetBibEntryWithDOI PrintBibliography
aboutUI <- function(request, id, datafolder, doc="doc.txt"){
  ns <- NS(id)
  tabPanel(title=div(selectInput('availableDatasets',
                                 label = NULL,
                                 choices = getDataSets(datafolder = datafolder),
                                 selected = NULL,
                                 width = "90vw")),
           includeHTML(doc),
           hr(),
           htmlOutput(ns("ref")),
           hr(),
           p(
             textInput(ns('search'), 'Search in database',
                       placeholder = 'Type key words here'),
             htmlOutput(ns('search_res'))
           ),
           hr(),
           div(class = "about-row-padding about-margin-bottom",
             div(
               class = "about-quarter",
               div(
                 class = "about-container about-red about-padding-16",
                 div(
                   class = "about-left",
                   icon('database', class="about-xxxlarge")
                 ),
                 div(
                   class = "about-right",
                   textOutput(ns('dataset_counts'))
                 ),
                 div(class="about-clear"),
                 h4("datasets")
               )
             ),
             div(
               class = "about-quarter",
               div(
                 class = "about-container about-orange about-padding-16",
                 div(
                   class = "about-left",
                   icon('sourcetree', class="about-xxxlarge")
                 ),
                 div(
                   class = "about-right",
                   textOutput(ns('reference_count'))
                 ),
                 div(class="about-clear"),
                 h4("references")
               )
             ),
             div(
               class = "about-quarter",
               div(
                 class = "about-container about-teal about-padding-16",
                 div(
                   class = "about-left",
                   icon('eye', class="about-xxxlarge")
                 ),
                 div(
                   class = "about-right",
                   textOutput(ns('visitor_count'))
                 ),
                 div(class="about-clear"),
                 h4("visitor")
               )
             ),
             div(
               class = "about-quarter",
               div(
                 class = "about-container about-blue about-padding-16",
                 div(
                   class = "about-left",
                   icon('fish', class="about-xxxlarge")
                 ),
                 div(
                   class = "about-right",
                   textOutput(ns('species_count'))
                 ),
                 div(class="about-clear"),
                 h4('species')
               )
             )
           ),
           hr(),
           h4("Full reference list:"),
           htmlOutput(ns('full_ref_list')),
           p(imageOutput('total_visitor', width = "50%", height="150px")),
           div(style = "display:none;",
               textInput("remote_agent", "remote_agent",
                         value = request[["HTTP_USER_AGENT"]]),
               visitorDependencies()
           ),
           hr(),
           p(em("This webpage was modified from "),
             a("ShinyCell",
               href = "https://github.com/SGDDNB/ShinyCell",target="_blank"))
  )
}
aboutServer <- function(id, dataSource, optCrt, currentdataset,
                        datafolder){
  moduleServer(id, function(input, output, session){
    ref <- getRef(currentdataset,
                  "bib",
                  getAppConf(datafolder))
    if(!is.na(ref)){
      output$ref <- renderUI(tagList(
        h4("Reference for current data"),
        p(ref)))
    }

    output$full_ref_list <- renderUI(
      get_full_ref_list(getAppConf(datafolder))
    )
    observeEvent(input$search, {
      if(input$search != '' && input$search != "Type key words here"){
        key_words = strsplit(input$search, '\\s+')[[1]]
        key_words = gsub("[^a-zA-Z0-9._-]+", "", key_words)
        key_words <- paste(key_words, collapse='|')
        res_data <- lapply(getAppConf(datafolder), function(.ele){
          if(grepl(key_words, paste(.ele$title, .ele$id, do.call(paste, .ele$ref)),
                   ignore.case = TRUE)){
            return(c(.ele$id, .ele$title))
          }else{
            return(NULL)
          }
        })
        ## update search_res
        res_data <- res_data[lengths(res_data)>0]
        if(!is.null(res_data)){
          output$search_res <- renderUI(HTML(
            paste("<ul>",
                  vapply(res_data, function(.ele){
                    return(paste0("<li><a href='?data=", .ele[1], "'>", .ele[2], "</a>"))
                  }, character(1L)),
                  "</ul>")
          ))
        }
      }
    })
    output$dataset_counts <- renderText({
      length(getDataSets(datafolder))
    })
    output$visitor_count <- renderText({
      counter <- read.delim("www/counter.tsv", header = TRUE)
      length(unique(counter$ip))
    })
    output$reference_count <- renderText({
      get_full_ref_list(getAppConf(datafolder), returnLen=TRUE)
    })
    output$species_count <- renderText({
      length(unique(lapply(getAppConf(datafolder), `[[`, i="species")))
    })
  })
}
updateVisitor <- function(input, output, session){
  conterFilename <- "www/counter.tsv"
  ## update visitor stats
  update_visitor <- function(){
    req(input$remote_addr)
    counter <- read.delim(conterFilename, header = TRUE)
    ips <- counter$ip
    counter <- as.Date(counter$date)
    visitors <- paste(format(counter, "%d/%m/%y %H"), ips)
    current <- Sys.time()
    ip <- isolate(input$remote_addr)
    agent <- isolate(input$remote_agent)
    if(!paste(format(current, "%d/%m/%y %H"), ip) %in% visitors){
      write(paste(as.character(current), ip, agent, sep="\t"),
            conterFilename, append = TRUE)
    }
  }
  observeEvent(input$remote_addr, update_visitor())
  output$total_visitor <- renderPlot({
    counter <- read.delim(conterFilename, header = TRUE)
    counter <- as.Date(counter$date)
    counter <- counter[as.numeric(difftime(as.Date(Sys.time()), counter, units = 'days'))<730]
    counter <- table(format(counter, "%y-%m"))
    counter <- as.data.frame(counter)
    ggplot(counter, aes(x=Var1, y=Freq)) +
      geom_bar(stat = "identity", fill="darkorchid4") +
      theme_minimal() + xlab("") + ylab("visitor counts") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
}
