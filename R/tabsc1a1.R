tab1a1 <- function(){
  tabPanel(
  HTML("CellInfo vs GeneExpr"),
  h4("Cell information vs gene expression on reduced dimensions"),
  "In this tab, users can visualise both cell information and gene ",
  "expression side-by-side on low-dimensional representions.",
  br(),br(),
  fluidRow(
    column(
      3, h4("Dimension Reduction"),
      fluidRow(
        column(
          12, selectInput("sc1a1drX", "X-axis:", choices = NULL),
          selectInput("sc1a1drY", "Y-axis:", choices = NULL))
      )
    ), # End of column (6 space)
    column(
      3, actionButton("sc1a1togL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.sc1a1togL % 2 == 0",
        selectInput("sc1a1sub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("sc1a1sub1.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("sc1a1tog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.sc1a1tog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("sc1a1siz", "Point size:",
                           min = 0, max = 4, value = 1.25, step = 0.25),
            radioButtons("sc1a1psz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("sc1a1fsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("sc1a1asp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("sc1a1txt", "Show axis text", value = FALSE)
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
          6, selectInput("sc1a1inp1", "Cell information:",
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
          6, actionButton("sc1a1tog1", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.sc1a1tog1 % 2 == 1",
            radioButtons("sc1a1col1", "Colour (Continuous data):",
                         choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                         selected = "Blue-Yellow-Red"),
            radioButtons("sc1a1ord1", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Original", inline = TRUE),
            checkboxInput("sc1a1lab1", "Show cell info labels", value = TRUE)
          )
        )
      ),
      fluidRow(column(12, uiOutput("sc1a1oup1.ui"))),
      downloadButton("sc1a1oup1.pdf", "Download PDF"),
      downloadButton("sc1a1oup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("sc1a1oup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("sc1a1oup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5)), br(),
      actionButton("sc1a1tog9", "Toggle to show cell numbers / statistics"),
      conditionalPanel(
        condition = "input.sc1a1tog9 % 2 == 1",
        h4("Cell numbers / statistics"),
        radioButtons("sc1a1splt", "Split continuous cell info into:",
                     choices = c("Quartile", "Decile"),
                     selected = "Decile", inline = TRUE),
        dataTableOutput("sc1a1.dt")
      )
    ), # End of column (6 space)
    column(
      6, h4("Gene expression"),
      fluidRow(
        column(
          6, selectInput("sc1a1inp2", "Gene name:", choices=NULL) %>%
            shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                   title = "Gene expression to colour cells by",
                   content = c("Select gene to colour cells by gene expression",
                               paste0("- Gene expression are coloured in a ",
                                      "White-Red colour scheme which can be ",
                                      "changed in the plot controls"),
                               paste("- Gene name support autocomplete.",
                                     "Try to input gene name in the input box.")))
        ),
        column(
          6, actionButton("sc1a1tog2", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.sc1a1tog2 % 2 == 1",
            radioButtons("sc1a1type2", "Plot type",
                         choices = c("Dotplot", "Ridgeplot"),
                         selected = "Dotplot"),
            conditionalPanel(
              condition = "input.sc1a1type2 == 'Dotplot'",
              radioButtons("sc1a1col2", "Colour:",
                           choices = c("White-Red","Blue-Yellow-Red","Yellow-Green-Purple"),
                           selected = "White-Red"),
              radioButtons("sc1a1ord2", "Plot order:",
                           choices = c("Max-1st", "Min-1st", "Original", "Random"),
                           selected = "Max-1st", inline = TRUE)),
            conditionalPanel(
              condition = "input.sc1a1type2 == 'Ridgeplot'",
              actionButton("sc1a1xlim0", "Manually set x axis", inline = TRUE),
              conditionalPanel(
                condition = "input.sc1a1xlim0 % 2 ==1",
                sliderInput("sc1a1xlim1", "Xlim range:",
                            min = -10, max = 100, value = c(0, 10),
                            step = 0.1))
            )
          )
        )
      ) ,
      fluidRow(column(12, uiOutput("sc1a1oup2.ui"))),
      downloadButton("sc1a1oup2.pdf", "Download PDF"),
      downloadButton("sc1a1oup2.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("sc1a1oup2.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("sc1a1oup2.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
