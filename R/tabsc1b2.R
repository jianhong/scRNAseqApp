tab1b2 <- function(){
  tabPanel(
  HTML("Gene coexpression"),
  h4("Coexpression of two genes on reduced dimensions"),
  "In this tab, users can visualise the coexpression of two genes ",
  "on low-dimensional representions.",
  br(),br(),
  fluidRow(
    column(
      3, h4("Dimension Reduction"),
      fluidRow(
        column(
          12, selectInput("sc1b2drX", "X-axis:", choices = NULL),
          selectInput("sc1b2drY", "Y-axis:", choices = NULL))
      )
    ), # End of column (6 space)
    column(
      3, actionButton("sc1b2togL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.sc1b2togL % 2 == 0",
        selectInput("sc1b2sub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("sc1b2sub1.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("sc1b2tog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.sc1b2tog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("sc1b2siz", "Point size:",
                           min = 0, max = 4, value = 1.5, step = 0.25),
            radioButtons("sc1b2psz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("sc1b2fsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("sc1b2asp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("sc1b2txt", "Show axis text", value = FALSE)
          )
        )
      )
    )  # End of column (6 space)
  ),   # End of fluidRow (4 space)
  fluidRow(
    column(
      3, style="border-right: 2px solid black", h4("Gene Expression"),
      selectInput("sc1b2inp1", "Gene 1:", choices=NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Gene expression to colour cells by",
               content = c("Select gene to colour cells by gene expression",
                           paste0("- Gene expression are coloured in a ",
                                  "White-Red colour scheme which can be ",
                                  "changed in the plot controls"))),
      selectInput("sc1b2inp2", "Gene 2:", choices=NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Gene expression to colour cells by",
               content = c("Select gene to colour cells by gene expression",
                           paste0("- Gene expression are coloured in a ",
                                  "White-Blue colour scheme which can be ",
                                  "changed in the plot controls"))),
      actionButton("sc1b2tog1", "Toggle plot controls"),
      conditionalPanel(
        condition = "input.sc1b2tog1 % 2 == 1",
        radioButtons("sc1b2col1", "Colour:",
                     choices = c("Red (Gene1); Blue (Gene2)",
                                 "Orange (Gene1); Blue (Gene2)",
                                 "Red (Gene1); Green (Gene2)",
                                 "Green (Gene1); Blue (Gene2)"),
                     selected = "Red (Gene1); Blue (Gene2)"),
        radioButtons("sc1b2ord1", "Plot order:",
                     choices = c("Max-1st", "Min-1st", "Original", "Random"),
                     selected = "Max-1st", inline = TRUE)
      )
    ), # End of column (6 space)
    column(
      6, style="border-right: 2px solid black",
      uiOutput("sc1b2oup1.ui"),
      downloadButton("sc1b2oup1.pdf", "Download PDF"),
      downloadButton("sc1b2oup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("sc1b2oup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5)),
      div(style="display:inline-block",
          numericInput("sc1b2oup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 10, step = 0.5))
    ), # End of column (6 space)
    column(
      3, uiOutput("sc1b2oup2.ui"),
      downloadButton("sc1b2oup2.pdf", "Download PDF"),
      downloadButton("sc1b2oup2.png", "Download PNG"),
      br(), h4("Cell numbers"),
      dataTableOutput("sc1b2.dt")
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
