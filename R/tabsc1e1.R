tab1e1 <- function(req){
  tabPanel(value="ChangeDataset",
         HTML("Change dataset"),
         h4("Change the dataset"),
         "In this tab, users can change the dataset.",
         br(),br(),
         selectInput("availableDatasets",
                     "Available datasets",
                     choices = datasets,
                     selected = "GSM5023610_glial_app",
                     width = 800),

         p(strong("Reference: "),
           textOutput('ref_author', inline = TRUE),
           textOutput('ref_title', inline = TRUE),
           em(textOutput('ref_journal', inline = TRUE)),
           textOutput('ref_year', inline = TRUE),
           htmlOutput("ref_pmid", inline = TRUE, target="_blank"),
           style = "font-size: 125%;"),
         br(), hr(), br(),
         includeHTML("inst/extdata/doc.txt"),
         br(), hr(), br(),
         p(imageOutput('total_visitor', width = "30%", height="100px")),
         div(style = "display:none;",
             textInput("remote_addr", "remote_addr", {
               if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
                 req[["HTTP_X_FORWARDED_FOR"]]
               else
                 req[["REMOTE_ADDR"]]
             }),
             textInput("remote_agent", "remote_agent",
                       value = req[["HTTP_USER_AGENT"]])),
         hr(),
         p(em("This webpage was modified from "),
           a("ShinyCell", href = "https://github.com/SGDDNB/ShinyCell",target="_blank"))
  )
}
