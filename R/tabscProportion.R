tabscProportion <- function(){
  tabPanel(
  HTML("Proportion plot"),
  h4("Proportion / cell numbers across different cell information"),
  "In this tab, users can visualise the composition of single cells based on one discrete ",
  "cell information across another discrete cell information. ",
  "Usage examples include the library or cellcycle composition across clusters.",
  br(),br(),
  fluidRow(
    column(
      3, style="border-right: 2px solid black",
      selectInput("scProportioninp1", "Cell information to plot (X-axis):",
                  choices = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Cell information to plot cells by",
               content = c("Select categorical cell information to plot cells by",
                           "- Plotted as the X-axis of the proportion plot")),
      selectInput("scProportioninp2", "Cell information to group / colour by:",
                  choices = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Cell information to group / colour cells by",
               content = c("Select categorical cell information to group / colour cells by",
                           "- Proportion / cell numbers are shown in different colours")),
      radioButtons("scProportiontyp", "Plot value:",
                   choices = c("Proportion", "CellNumbers"),
                   selected = "Proportion", inline = TRUE),
      checkboxInput("scProportionflp", "Flip X/Y", value = FALSE),
      actionButton("scProportiontog", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.scProportiontog % 2 == 1",
        radioButtons("scProportionpsz", "Plot size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE),
        radioButtons("scProportionfsz", "Font size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE))
    ), # End of column (6 space)
    column(9, uiOutput("scProportionoup.ui"),
           downloadButton("scProportionoup.pdf", "Download PDF"),
           downloadButton("scProportionoup.png", "Download PNG"), br(),
           div(style="display:inline-block",
               numericInput("scProportionoup.h", "PDF / PNG height:", width = "138px",
                            min = 4, max = 20, value = 8, step = 0.5)),
           div(style="display:inline-block",
               numericInput("scProportionoup.w", "PDF / PNG width:", width = "138px",
                            min = 4, max = 20, value = 10, step = 0.5)), br(),
           actionButton("scProportiontog9", "Toggle to show statistics"),
           conditionalPanel(
             condition = "input.scProportiontog9 % 2 == 1",
             h4("Statistics"),
             dataTableOutput("scProportion.dt")
           )
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
