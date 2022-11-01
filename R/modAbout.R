#' @importFrom htmltools htmlDependency
visitorDependencies <- function(){
  htmlDependency(name = "scRNAseqApp-assets", version = "0.0.1",
                 package = "scRNAseqApp",
                 src = "assets",
                 script = "js/login.js",
                 stylesheet = c()
  )
}
#' @importFrom bibtex read.bib
#' @importFrom RefManageR GetBibEntryWithDOI PrintBibliography
aboutUI <- function(req, id, doc="doc.txt"){
  ns <- NS(id)
  tabPanel(value="About",
           HTML("About"),
           h4("About the dataset"),
           "In this tab will introduce the details about current dataset.",
           "And the details about the database.",
           br(),br(),

           textOutput(ns("ref")),
           br(), hr(), br(),
           includeHTML(doc),
           br(), hr(), br(),
           h4("Full reference list:"),
           htmlOutput(ns('full_ref_list')),
           p(imageOutput('total_visitor', width = "50%", height="150px")),
           div(style = "display:none;",
               textInput("remote_agent", "remote_agent",
                         value = req[["HTTP_USER_AGENT"]]),
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
    output$ref <- renderText(getRef(currentdataset,
                                    "bib",
                                    getAppConf(datafolder)))
    output$full_ref_list <- renderUI(
      get_full_ref_list(getAppConf(datafolder))
    )
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
