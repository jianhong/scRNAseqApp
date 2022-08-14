tabscCoExpr <- function(){
  tabPanel(
  htmlOutput("tabCoExpr"),
  htmlOutput("tabCoExprSubTitle"),
  "In this tab, users can visualise the coexpression of two genes ",
  "on low-dimensional representions.",
  br(),br(),
  fluidRow(
    column(
      3, h4("Dimension Reduction"),
      fluidRow(
        column(
          12, selectInput("scCoExprdrX", "X-axis:", choices = NULL),
          selectInput("scCoExprdrY", "Y-axis:", choices = NULL))
      )
    ), # End of column (6 space)
    column(
      3, actionButton("scCoExprtogL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.scCoExprtogL % 2 == 0",
        selectInput("scCoExprsub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("scCoExprsub1.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("scCoExprtog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.scCoExprtog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("scCoExprsiz", "Point size:",
                           min = 0, max = 4, value = 1.5, step = 0.25),
            radioButtons("scCoExprpsz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("scCoExprfsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("scCoExprasp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("scCoExprtxt", "Show axis text", value = FALSE)
          )
        )
      )
    )  # End of column (6 space)
  ),   # End of fluidRow (4 space)
  fluidRow(
    column(
      3, style="border-right: 2px solid black", htmlOutput("tabCoExprSubTit1"),
      selectInput("scCoExprinp1", "Gene 1:", choices=NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Gene expression to colour cells by",
               content = c("Select gene to colour cells by gene expression",
                           paste0("- Gene expression are coloured in a ",
                                  "White-Red colour scheme which can be ",
                                  "changed in the plot controls"),
                           paste("- Gene name support autocomplete.",
                                 "Try to input gene name in the input box."))),
      selectInput("scCoExprinp2", "Gene 2:", choices=NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Gene expression to colour cells by",
               content = c("Select gene to colour cells by gene expression",
                           paste0("- Gene expression are coloured in a ",
                                  "White-Blue colour scheme which can be ",
                                  "changed in the plot controls"),
                           paste("- Gene name support autocomplete.",
                                 "Try to input gene name in the input box."))),
      actionButton("scCoExprtog1", "Toggle plot controls"),
      conditionalPanel(
        condition = "input.scCoExprtog1 % 2 == 1",
        radioButtons("scCoExprcol1", "Colour:",
                     choices = c("Red (Gene1); Blue (Gene2)",
                                 "Orange (Gene1); Blue (Gene2)",
                                 "Red (Gene1); Green (Gene2)",
                                 "Green (Gene1); Blue (Gene2)"),
                     selected = "Red (Gene1); Blue (Gene2)"),
        radioButtons("scCoExprord1", "Plot order:",
                     choices = c("Max-1st", "Min-1st", "Original", "Random"),
                     selected = "Max-1st", inline = TRUE)
      )
    ), # End of column (6 space)
    column(
      6, style="border-right: 2px solid black",
      uiOutput("scCoExproup1.ui"),
      downloadButton("scCoExproup1.pdf", "Download PDF"),
      downloadButton("scCoExproup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("scCoExproup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5)),
      div(style="display:inline-block",
          numericInput("scCoExproup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 10, step = 0.5))
    ), # End of column (6 space)
    column(
      3, uiOutput("scCoExproup2.ui"),
      downloadButton("scCoExproup2.pdf", "Download PDF"),
      downloadButton("scCoExproup2.png", "Download PNG"),
      br(), h4("Cell numbers"),
      dataTableOutput("scCoExpr.dt")
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
