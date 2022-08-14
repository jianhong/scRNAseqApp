tabgeneExprGeneExpr <- function(){
  tabPanel(
    htmlOutput("tabGeneExprGeneExpr"),
    htmlOutput("tabGeneExprGeneExprSubTitle"),
  "In this tab, users can visualise two gene expressions side-by-side ",
  "on low-dimensional representions.",
  br(),br(),
  fluidRow(
    column(
      3, h4("Dimension Reduction"),
      fluidRow(
        column(
          12, selectInput("geneExprGeneExprdrX", "X-axis:", choices = NULL),
          selectInput("geneExprGeneExprdrY", "Y-axis:", choices = NULL))
      )
    ), # End of column (6 space)
    column(
      3, actionButton("geneExprGeneExprtogL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.geneExprGeneExprtogL % 2 == 0",
        selectInput("geneExprGeneExprsub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("geneExprGeneExprsub1.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("geneExprGeneExprtog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.geneExprGeneExprtog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("geneExprGeneExprsiz", "Point size:",
                           min = 0, max = 4, value = 1.25, step = 0.25),
            radioButtons("geneExprGeneExprpsz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("geneExprGeneExprfsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("geneExprGeneExprasp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("geneExprGeneExprtxt", "Show axis text", value = FALSE)
          )
        )
      )
    )  # End of column (6 space)
  ),   # End of fluidRow (4 space)
  fluidRow(
    column(
      6, style="border-right: 2px solid black", htmlOutput("tabGeneExprGeneExprSub1"),
      fluidRow(
        column(
          6, selectInput("geneExprGeneExprinp1", "Gene name:", choices=NULL) %>%
            shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                   title = "Gene information to colour cells by",
                   content = c("Select gene to colour cells by gene expression/accessibility",
                               paste0("- Gene expression/accessibility are coloured in a ",
                                      "White-Red colour scheme which can be ",
                                      "changed in the plot controls"),
                               paste("- Gene name support autocomplete.",
                                     "Try to input gene name in the input box.")))
        ),
        column(
          6, actionButton("geneExprGeneExprtog1", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.geneExprGeneExprtog1 % 2 == 1",
            radioButtons("geneExprGeneExprcol1", "Colour:",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "White-Red"),
            radioButtons("geneExprGeneExprord1", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Max-1st", inline = TRUE),
            actionButton("geneExprGeneExprrg0", "Manually set max color value", inline = TRUE),
            conditionalPanel(
              condition = "input.geneExprGeneExprrg0 % 2 ==1",
              numericInput("geneExprGeneExprrg1", "Max value:", value = 100))
          )
        )
      ),
      fluidRow(column(12, uiOutput("geneExprGeneExproup1.ui"))),
      downloadButton("geneExprGeneExproup1.pdf", "Download PDF"),
      downloadButton("geneExprGeneExproup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("geneExprGeneExproup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("geneExprGeneExproup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    ), # End of column (6 space)
    column(
      6, htmlOutput("tabGeneExprGeneExprSub2"),
      fluidRow(
        column(
          6, selectInput("geneExprGeneExprinp2", "Gene name:", choices=NULL) %>%
            shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                   title = "Gene information to colour cells by",
                   content = c("Select gene to colour cells by gene expression/accessibility",
                               paste0("- Gene expression/accessibility are coloured in a ",
                                      "White-Red colour scheme which can be ",
                                      "changed in the plot controls"),
                               paste("- Gene name support autocomplete.",
                                     "Try to input gene name in the input box.")))
        ),
        column(
          6, actionButton("geneExprGeneExprtog2", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.geneExprGeneExprtog2 % 2 == 1",
            radioButtons("geneExprGeneExprcol2", "Colour:",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "White-Red"),
            radioButtons("geneExprGeneExprord2", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Max-1st", inline = TRUE),
            actionButton("geneExprGeneExprrg2", "Manually set max color value", inline = TRUE),
            conditionalPanel(
              condition = "input.geneExprGeneExprrg2 % 2 ==1",
              numericInput("geneExprGeneExprrg3", "Max value:", value = 100))
          )
        )
      ),
      fluidRow(column(12, uiOutput("geneExprGeneExproup2.ui"))),
      downloadButton("geneExprGeneExproup2.pdf", "Download PDF"),
      downloadButton("geneExprGeneExproup2.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("geneExprGeneExproup2.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("geneExprGeneExproup2.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
