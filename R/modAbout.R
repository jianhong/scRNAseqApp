#' @importFrom bibtex read.bib
#' @importFrom RefManageR GetBibEntryWithDOI PrintBibliography
aboutUI <- function(req, id){
  ns <- NS(id)
  tabPanel(value="About",
           HTML("About"),
           h4("About the dataset"),
           "In this tab will introduce the details about current dataset.",
           "And the details about the database.",
           br(),br(),

           textOutput(ns("ref")),
           br(), hr(), br(),
           includeHTML("doc.txt"),
           br(), hr(), br(),
           p(imageOutput('total_visitor', width = "30%", height="100px")),
           div(style = "display:none;",
               textInput("remote_agent", "remote_agent",
                         value = req[["HTTP_USER_AGENT"]])),
           hr(),
           p(em("This webpage was modified from "),
             a("ShinyCell", href = "https://github.com/SGDDNB/ShinyCell",target="_blank"))
  )
}
aboutServer <- function(id, dataSource, optCrt, currentdataset,
                        datafolder){
  moduleServer(id, function(input, output, session){
    output$ref <- renderText(getRef(currentdataset, "bib", getAppConf(datafolder)))
  })
}
