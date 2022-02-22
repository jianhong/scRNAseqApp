tab1a2 <- function(){
  tabPanel(
  HTML("CellInfo vs CellInfo"),
  h4("Cell information vs cell information on dimension reduction"),
  "In this tab, users can visualise two cell informations side-by-side ",
  "on low-dimensional representions.",
  br(),br(),
  fluidRow(
    column(
      3, h4("Dimension Reduction"),
      fluidRow(
        column(
          12, selectInput("sc1a2drX", "X-axis:", choices = NULL),
          selectInput("sc1a2drY", "Y-axis:", choices = NULL))
      )
    ), # End of column (6 space)
    column(
      3, actionButton("sc1a2togL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.sc1a2togL % 2 == 0",
        selectInput("sc1a2sub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("sc1a2sub1.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("sc1a2tog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.sc1a2tog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("sc1a2siz", "Point size:",
                           min = 0, max = 4, value = 1.25, step = 0.25),
            radioButtons("sc1a2psz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("sc1a2fsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("sc1a2asp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("sc1a2txt", "Show axis text", value = FALSE)
          )
        )
      )
    )  # End of column (6 space)
  ),   # End of fluidRow (4 space)
  fluidRow(
    column(
      6, style="border-right: 2px solid black", h4("Cell information 1"),
      fluidRow(
        column(
          6, selectInput("sc1a2inp1", "Cell information:",
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
          6, actionButton("sc1a2tog1", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.sc1a2tog1 % 2 == 1",
            radioButtons("sc1a2col1", "Colour (Continuous data):",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "Blue-Yellow-Red"),
            radioButtons("sc1a2ord1", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Original", inline = TRUE),
            checkboxInput("sc1a2lab1", "Show cell info labels", value = TRUE)
          )
        )
      ),
      fluidRow(column(12, uiOutput("sc1a2oup1.ui"))),
      downloadButton("sc1a2oup1.pdf", "Download PDF"),
      downloadButton("sc1a2oup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("sc1a2oup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("sc1a2oup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    ), # End of column (6 space)
    column(
      6, h4("Cell information 2"),
      fluidRow(
        column(
          6, selectInput("sc1a2inp2", "Cell information:",
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
          6, actionButton("sc1a2tog2", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.sc1a2tog2 % 2 == 1",
            radioButtons("sc1a2col2", "Colour (Continuous data):",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "Blue-Yellow-Red"),
            radioButtons("sc1a2ord2", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Original", inline = TRUE),
            checkboxInput("sc1a2lab2", "Show cell info labels", value = TRUE)
          )
        )
      ),
      fluidRow(column(12, uiOutput("sc1a2oup2.ui"))),
      downloadButton("sc1a2oup2.pdf", "Download PDF"),
      downloadButton("sc1a2oup2.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("sc1a2oup2.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("sc1a2oup2.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
