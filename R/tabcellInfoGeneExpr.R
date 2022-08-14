tabcellInfoGeneExpr <- function(){
  tabPanel(
    htmlOutput("tabCellInfoGeneExpr"),
    htmlOutput("tabCellInfoGeneExprSubTitle"),
  "In this tab, users can visualise both cell and feature information ",
  "side-by-side on low-dimensional representions.",
  br(),br(),
  fluidRow(
    column(
      3, h4("Dimension Reduction"),
      fluidRow(
        column(
          12, selectInput("cellInfoGeneExprdrX", "X-axis:", choices = NULL),
          selectInput("cellInfoGeneExprdrY", "Y-axis:", choices = NULL))
      )
    ), # End of column (6 space)
    column(
      3, actionButton("cellInfoGeneExprtogL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.cellInfoGeneExprtogL % 2 == 0",
        selectInput("cellInfoGeneExprsub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("cellInfoGeneExprsub1.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("cellInfoGeneExprtog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.cellInfoGeneExprtog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("cellInfoGeneExprsiz", "Point size:",
                           min = 0, max = 4, value = 1.25, step = 0.25),
            radioButtons("cellInfoGeneExprpsz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("cellInfoGeneExprfsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("cellInfoGeneExprasp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("cellInfoGeneExprtxt", "Show axis text", value = FALSE)
          )
        )
      ) # End of conditionalPanel
    )  # End of column (6 space)
  ),   # End of fluidRow (4 space)
  fluidRow(
    column(
      6, style="border-right: 2px solid black", h4("Cell information"),
      fluidRow(
        column(
          6, selectInput("cellInfoGeneExprinp1", "Cell information:",
                         choices = NULL) %>%
            shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                   title = "Cell information to colour cells by",
                   content = c("Select cell information to colour cells",
                               "- Categorical covariates have a fixed colour palette",
                               paste0("- Continuous covariates are coloured in a ",
                                      "Blue-Yellow-Red colour scheme, which can be ",
                                      "changed in the plot controls")))
        ),
        column(
          6, actionButton("cellInfoGeneExprtog1", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.cellInfoGeneExprtog1 % 2 == 1",
            radioButtons("cellInfoGeneExprcol1", "Colour (Continuous data):",
                         choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                         selected = "Blue-Yellow-Red"),
            radioButtons("cellInfoGeneExprord1", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Original", inline = TRUE),
            checkboxInput("cellInfoGeneExprlab1", "Show cell info labels", value = TRUE)
          )
        )
      ),
      fluidRow(column(12, uiOutput("cellInfoGeneExproup1.ui"))),
      downloadButton("cellInfoGeneExproup1.pdf", "Download PDF"),
      downloadButton("cellInfoGeneExproup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("cellInfoGeneExproup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("cellInfoGeneExproup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5)), br(),
      actionButton("cellInfoGeneExprtog9", "Toggle to show cell numbers / statistics"),
      conditionalPanel(
        condition = "input.cellInfoGeneExprtog9 % 2 == 1",
        h4("Cell numbers / statistics"),
        radioButtons("cellInfoGeneExprsplt", "Split continuous cell info into:",
                     choices = c("Quartile", "Decile"),
                     selected = "Decile", inline = TRUE),
        dataTableOutput("cellInfoGeneExpr.dt")
      )
    ), # End of column (6 space)
    column(
      6, htmlOutput("tabCellInfoGeneExprSubTitGene"),
      fluidRow(
        column(
          6, selectInput("cellInfoGeneExprinp2", "Gene name:", choices=NULL) %>%
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
          6, actionButton("cellInfoGeneExprtog2", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.cellInfoGeneExprtog2 % 2 == 1",
            radioButtons("cellInfoGeneExprtype2", "Plot type",
                         choices = c("Dotplot", "Ridgeplot"),
                         selected = "Dotplot"),
            conditionalPanel(
              condition = "input.cellInfoGeneExprtype2 == 'Dotplot'",
              radioButtons("cellInfoGeneExprcol2", "Colour:",
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                           selected = "White-Red"),
              radioButtons("cellInfoGeneExprord2", "Plot order:",
                           choices = c("Max-1st", "Min-1st", "Original", "Random"),
                           selected = "Max-1st", inline = TRUE),
              actionButton("cellInfoGeneExprrg0", "Manually set max color value", inline = TRUE),
              conditionalPanel(
                condition = "input.cellInfoGeneExprrg0 % 2 ==1",
                numericInput("cellInfoGeneExprrg1", "Max value:", value = 100))
              ),
            conditionalPanel(
              condition = "input.cellInfoGeneExprtype2 == 'Ridgeplot'",
              actionButton("cellInfoGeneExprxlim0", "Manually set x axis", inline = TRUE),
              conditionalPanel(
                condition = "input.cellInfoGeneExprxlim0 % 2 ==1",
                sliderInput("cellInfoGeneExprxlim1", "Xlim range:",
                            min = -10, max = 100, value = c(0, 10),
                            step = 0.1))
            )
          )
        )
      ) ,
      fluidRow(column(12, uiOutput("cellInfoGeneExproup2.ui"))),
      downloadButton("cellInfoGeneExproup2.pdf", "Download PDF"),
      downloadButton("cellInfoGeneExproup2.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("cellInfoGeneExproup2.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("cellInfoGeneExproup2.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}

