tabcellInfoCellInfo <- function(){
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
          12, selectInput("cellInfoCellInfodrX", "X-axis:", choices = NULL),
          selectInput("cellInfoCellInfodrY", "Y-axis:", choices = NULL))
      )
    ), # End of column (6 space)
    column(
      3, actionButton("cellInfoCellInfotogL", "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.cellInfoCellInfotogL % 2 == 0",
        selectInput("cellInfoCellInfosub1", "Cell information to subset:",
                    choices = NULL),
        uiOutput("cellInfoCellInfosub1.ui")
      )
    ), # End of column (6 space)
    column(
      6, actionButton("cellInfoCellInfotog0", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.cellInfoCellInfotog0 % 2 == 1",
        fluidRow(
          column(
            6, sliderInput("cellInfoCellInfosiz", "Point size:",
                           min = 0, max = 4, value = 1.25, step = 0.25),
            radioButtons("cellInfoCellInfopsz", "Plot size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE),
            radioButtons("cellInfoCellInfofsz", "Font size:",
                         choices = c("Small", "Medium", "Large"),
                         selected = "Medium", inline = TRUE)
          ),
          column(
            6, radioButtons("cellInfoCellInfoasp", "Aspect ratio:",
                            choices = c("Square", "Fixed", "Free"),
                            selected = "Square", inline = TRUE),
            checkboxInput("cellInfoCellInfotxt", "Show axis text", value = FALSE)
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
          6, selectInput("cellInfoCellInfoinp1", "Cell information:",
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
          6, actionButton("cellInfoCellInfotog1", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.cellInfoCellInfotog1 % 2 == 1",
            radioButtons("cellInfoCellInfocol1", "Colour (Continuous data):",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "Blue-Yellow-Red"),
            radioButtons("cellInfoCellInfoord1", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Original", inline = TRUE),
            checkboxInput("cellInfoCellInfolab1", "Show cell info labels", value = TRUE)
          )
        )
      ),
      fluidRow(column(12, uiOutput("cellInfoCellInfooup1.ui"))),
      downloadButton("cellInfoCellInfooup1.pdf", "Download PDF"),
      downloadButton("cellInfoCellInfooup1.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("cellInfoCellInfooup1.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("cellInfoCellInfooup1.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    ), # End of column (6 space)
    column(
      6, h4("Cell information 2"),
      fluidRow(
        column(
          6, selectInput("cellInfoCellInfoinp2", "Cell information:",
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
          6, actionButton("cellInfoCellInfotog2", "Toggle plot controls"),
          conditionalPanel(
            condition = "input.cellInfoCellInfotog2 % 2 == 1",
            radioButtons("cellInfoCellInfocol2", "Colour (Continuous data):",
                         choices = c("White-Red", "Blue-Yellow-Red",
                                     "Yellow-Green-Purple"),
                         selected = "Blue-Yellow-Red"),
            radioButtons("cellInfoCellInfoord2", "Plot order:",
                         choices = c("Max-1st", "Min-1st", "Original", "Random"),
                         selected = "Original", inline = TRUE),
            checkboxInput("cellInfoCellInfolab2", "Show cell info labels", value = TRUE)
          )
        )
      ),
      fluidRow(column(12, uiOutput("cellInfoCellInfooup2.ui"))),
      downloadButton("cellInfoCellInfooup2.pdf", "Download PDF"),
      downloadButton("cellInfoCellInfooup2.png", "Download PNG"), br(),
      div(style="display:inline-block",
          numericInput("cellInfoCellInfooup2.h", "PDF / PNG height:", width = "138px",
                       min = 4, max = 20, value = 6, step = 0.5)),
      div(style="display:inline-block",
          numericInput("cellInfoCellInfooup2.w", "PDF / PNG width:", width = "138px",
                       min = 4, max = 20, value = 8, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
