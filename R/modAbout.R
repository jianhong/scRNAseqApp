#' @importFrom htmltools htmlDependency
visitorDependencies <- function(){
  htmlDependency(name = "scRNAseqApp-assets", version = "0.0.1",
                 package = "scRNAseqApp",
                 src = "assets",
                 script = c("js/script.js"),
                 stylesheet = c("css/style.css")
  )
}
summaryBox <- function(title, value, width = 4, icon = "fas fa-chart-bar", style = "info", border = "left") {
  div(
    class = c(paste0("col-md-", width), "about-left-border"),
    div(
      class = paste0("card border-", border, "-", style, " shadow h-100 py-2"),
      div(
        class = "card-body",
        div(
          class = "row no-gutters align-items-center",
          div(
            class = "col mr-2",
            div(
              class = paste0("text-xs font-weight-bold text-", style, " text-uppercase mb-1"),
              toupper(title)
            ),
            div(
              class = "h5 mb-0 font-weight-bold text-gray-800",
              value
            )
          ),
          div(
            class = "col-auto about-large-icon about-right",
            icon(class=paste0("about-", style), icon)
          )
        )
      )
    )
  )
}
homeUI <- function(){
  tabPanel(## fack home
    title = "",
    value = "home",
    icon = icon("home")
  )
}

#' @importFrom bibtex read.bib
#' @importFrom RefManageR GetBibEntryWithDOI PrintBibliography
aboutUI <- function(request, id, datafolder, banner, doc="doc.txt"){
  ns <- NS(id)
  # addResourcePath(prefix="pics",
  #                 directoryPath = system.file(
  #                   "assets", "images", package="scRNAseqApp"))
  tabPanel(title=div(selectInput('selectedDatasets',
                                 label = NULL,
                                 choices = getDataSets(datafolder = datafolder),
                                 selected = NULL,
                                 width = "90vw")),
           value = 'about',
           includeHTML(doc),
           hr(),
           htmlOutput(ns("ref")),
           hr(),
           div(
             class="about-display-container about-content about-top",
             style='width:100%;min-height:300px;',
             img(class="about-image about-glass", src=banner),
             absolutePanel(
               top = "15%",
               left = "10%",
               width = "80%",
               draggable = TRUE,
               div(class =
                     "panel panel-danger about-glass",
                   div(class =
                         "panel-heading about-container about-padding-16",
                       textInput(ns('search'), label=NULL,
                                 width = "96%",
                                 placeholder = 'Search the database')),
                   div(class = "panel-body about-bar border-bottom-info
                       shadow h-100 py-2",
                       htmlOutput(ns('search_res'))
                   )
               )
             )
           ),
           hr(),
           div(
             class="about-container",
             summaryBox("datasets", textOutput(ns('dataset_counts')), width = 3, icon = "database", style = "success", border = "bottom"),
             summaryBox("references", textOutput(ns('reference_count')), width = 3, icon = "book-open", style = "danger", border = "bottom"),
             summaryBox("visitors", textOutput(ns('visitor_count')), width = 3, icon = "eye", style = "primary", border = "bottom"),
             summaryBox("species", textOutput(ns('species_count')), width = 3, icon = "fish", style = "warning", border = "bottom")
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
    bibentry <- getRef(currentdataset,
                       "entry",
                       getAppConf(datafolder))
    if(is(bibentry, "bibentry")){
      output$ref <- renderUI(tagList(
        if(!is.null(bibentry$abstract)){
          if(!grepl("^The abstract of a scientific paper represents a concise",
                    bibentry$abstract)){
            tagList(
              h5("Details for current data"),
              HTML(format(bibentry$abstract, style="html")))
          }else{
            h5("Reference for current data")
          }
        }else{
          h5("Reference for current data")
        },
        HTML(format(bibentry, style="html"))
      ))
    }else{
      ref <- getRef(currentdataset,
                    "bib",
                    getAppConf(datafolder))
      if(!is.na(ref)){
        output$ref <- renderUI(tagList(
          h5("Reference for current data"),
          HTML(ref)))
      }
    }
    output$full_ref_list <- renderUI(
      get_full_ref_list(getAppConf(datafolder))
    )
    observeEvent(input$search, {
      if(input$search != '' && input$search != "Type key words here"){
        output$search_res <- renderUI(tags$div('searching...'))
        key_words <- strsplit(input$search, '\\s+')[[1]]
        updateSearch(key_words,
                     datafolder,
                     output,
                     dataSource()$symbolDict,
                     id)
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

updateSearch <- function(key_words, datafolder, output, symbolDict, id){
  key_words = gsub("[^a-zA-Z0-9._-]+", "", key_words)
  if(length(key_words)==1 && nchar(key_words)>1 && isGene(key_words, symbolDict)){## check if it is a gene
    search_res <- checkGene(key_words, datafolder, id=id)
    output$search_res <-
      renderUI(search_res$UI)
    for(i in seq_along(search_res$PLOT)){
      local({
        output[[names(search_res$PLOT)[i]]] <- search_res$PLOT[[i]]
      })
    }
  }else{
    res_data <- lapply(getAppConf(datafolder), function(.ele){
      x <- paste(unlist(.ele), collapse = " ")
      m <- vapply(key_words, grepl, logical(1L), x = x, ignore.case=TRUE)
      m <- sum(m)
      return(c(m, .ele$id, .ele$title))
    })
    ## update search_res
    res_data <- do.call(rbind, res_data)
    res_data <- res_data[res_data[, 1]>0, , drop=FALSE]
    res_data <- res_data[order(res_data[, 1], decreasing = TRUE),
                         -1, drop=FALSE]
    if(nrow(res_data)>0){
      output$search_res <- renderUI(
        tags$ul(class='about-ul',
                apply(res_data, 1, function(.ele){
                  return(tags$li(
                    tags$a(href=paste0('?data=', .ele[1]),
                           .ele[2])))
                })
        )
      )
    }else{
      output$search_res <- renderUI(tags$div("No records."))
    }
  }
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
    ggplot(counter, aes_string(x="Var1", y="Freq")) +
      geom_bar(stat = "identity", fill="darkorchid4") +
      theme_minimal() + xlab("") + ylab("visitor counts") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
}
