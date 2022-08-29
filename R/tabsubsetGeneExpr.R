tabsubsetGeneExpr <- function(){
  tabPanel(
    htmlOutput("tabSubsetGeneExpr"),
    htmlOutput("tabSubsetGeneExprSubTitle"),
  "In this tab, users can visualise gene expressions of two groups side-by-side ",
  "on low-dimensional representions.",
  br(),br(),
  fluidRow(
    column(
      3, h4("Dimension Reduction"),
      fluidRow(
        column(
          12, selectInput("subsetGeneExprdrX", "X-axis:", choices = NULL),
          selectInput("subsetGeneExprdrY", "Y-axis:", choices = NULL))
      ), h4("Information to show"),
      selectInput("subsetGeneExprinp1", "Gene name:", choices=NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                            title = "Gene information to colour cells by",
                            content = c("Select gene to colour cells by gene expression/accessibility",
                                        paste0("- Gene expression/accessibility are coloured in a ",
                                               "White-Red colour scheme which can be ",
                                               "changed in the plot controls"),
                                        paste("- Gene name support autocomplete.",
                                              "Try to input gene name in the input box."))),
      selectInput("subsetGeneExprinp2", "Cell information to show:",
                  choices = NULL)
    ), # End of column (6 space)
    column(
      3, actionButton("subsetGeneExprtogL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.subsetGeneExprtogL % 2 == 0",
        selectInput("subsetGeneExprsub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("subsetGeneExprsub1.ui"),
        selectInput("subsetGeneExprsub2", "Cell Info/Gene name to subset:",
                    choices = NULL),
        uiOutput("subsetGeneExprsub2.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("subsetGeneExprtog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.subsetGeneExprtog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("subsetGeneExprsiz", "Point size:",
                           min = 0, max = 4, value = 1.25, step = 0.25),
            radioButtons("subsetGeneExprpsz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("subsetGeneExprfsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("subsetGeneExprasp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("subsetGeneExprtxt", "Show axis text", value = FALSE)
          )
        )
      )
    )  # End of column (6 space)
  ),   # End of fluidRow (4 space)
  fluidRow(
    column(
      6, style="border-right: 2px solid black", htmlOutput("tabSubsetGeneExprSub1"),
      fluidRow(
        column(
          6, uiOutput("subsetGeneExprgrp1.ui")
        ),
        column(
          6, actionButton("subsetGeneExprtog1", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.subsetGeneExprtog1 % 2 == 1",
            radioButtons("subsetGeneExprcol1", "Colour:",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "White-Red"),
            radioButtons("subsetGeneExprord1", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Max-1st", inline = TRUE),
            actionButton("subsetGeneExprrg0", "Manually set max color value", inline = TRUE),
            conditionalPanel(
              condition = "input.subsetGeneExprrg0 % 2 ==1",
              numericInput("subsetGeneExprrg1", "Max value:", value = 100))
          )
        )
      ),
      fluidRow(column(12, uiOutput("subsetGeneExproup1.ui"))),
      downloadButton("subsetGeneExproup1.pdf", "Download PDF"),
      downloadButton("subsetGeneExproup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("subsetGeneExproup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("subsetGeneExproup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    ), # End of column (6 space)
    column(
      6, htmlOutput("tabSubsetGeneExprSub2"),
      fluidRow(
        column(
          6, uiOutput("subsetGeneExprgrp2.ui")
        ),
        column(
          6, actionButton("subsetGeneExprtog2", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.subsetGeneExprtog2 % 2 == 1",
            radioButtons("subsetGeneExprcol2", "Colour:",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "White-Red"),
            radioButtons("subsetGeneExprord2", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Max-1st", inline = TRUE),
            actionButton("subsetGeneExprrg2", "Manually set max color value", inline = TRUE),
            conditionalPanel(
              condition = "input.subsetGeneExprrg2 % 2 ==1",
              numericInput("subsetGeneExprrg3", "Max value:", value = 100))
          )
        )
      ),
      fluidRow(column(12, uiOutput("subsetGeneExproup2.ui"))),
      downloadButton("subsetGeneExproup2.pdf", "Download PDF"),
      downloadButton("subsetGeneExproup2.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("subsetGeneExproup2.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("subsetGeneExproup2.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
