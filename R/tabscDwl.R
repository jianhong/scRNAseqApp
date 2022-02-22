tabDownloader <- tabPanel(
  value="DownloadDataset",
  HTML("Download dataset"),
  h4("Download the dataset"),
  "In this tab, users can download the dataset.",
  br(),br(),
  fluidRow(
    column(
      3, style="border-right: 2px solid black",
      selectInput("scDwl1inp1", "Cell information:",
                  choices = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Cell information to group cells by",
               content = c("Select categorical cell information to group cells by")),
      selectInput("scDwl1inp1a", "Cell information to subset:",
                  choices = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Cell information to subset by:",
               content = c("Select categorical cell information to sebset cells by",
                           "- cells are shown in different subsets")),
      #uiOutput("scDwl1inp1b.ui"),
      selectInput("scDwl1inp2", "Cell Info:", choices=NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Cell Info",
               content = c("Select cell info  to download"))
    )#,
    #column(9, dataTableOutput("scDwl.dt") )
  )
)
