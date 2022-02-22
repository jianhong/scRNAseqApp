tab1d1 <- function(){
  tabPanel(
  HTML("Bubbleplot / Heatmap"),
  h4("Gene expression bubbleplot / heatmap"),
  "In this tab, users can visualise the gene expression patterns of ",
  "multiple genes grouped by categorical cell information (e.g. library / cluster).", br(),
  "The normalised expression are averaged, log-transformed and then plotted.",
  br(),br(),
  fluidRow(
    column(
      3, style="border-right: 2px solid black",
      textAreaInput("sc1d1inp", HTML("List of gene names <br />
                                         (Max 500 genes (over 50 will response slow), <br />
                                         separated by , or ; or newline):"),
                    height = "200px",
                    value = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "List of genes to plot on bubbleplot / heatmap",
               content = c("Input genes to plot",
                           "- Maximum 500 genes (due to ploting space limitations)",
                           "- Genes should be separated by comma, semicolon or newline")),
      selectInput("sc1d1grp", "Group by:",
                  choices = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Cell information to group cells by",
               content = c("Select categorical cell information to group cells by",
                           "- Single cells are grouped by this categorical covariate",
                           "- Plotted as the X-axis of the bubbleplot / heatmap")),
      radioButtons("sc1d1plt", "Plot type:",
                   choices = c("Bubbleplot", "Heatmap"),
                   selected = "Bubbleplot", inline = TRUE),
      checkboxInput("sc1d1scl", "Scale gene expression", value = TRUE),
      checkboxInput("sc1d1row", "Cluster rows (genes)", value = TRUE),
      checkboxInput("sc1d1col", "Cluster columns (samples)", value = FALSE),
      br(),
      actionButton("sc1d1tog", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.sc1d1tog % 2 == 1",
        radioButtons("sc1d1cols", "Colour scheme:",
                     choices = c("White-Red", "Blue-Yellow-Red",
                                 "Yellow-Green-Purple"),
                     selected = "Blue-Yellow-Red"),
        radioButtons("sc1d1psz", "Plot size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE),
        radioButtons("sc1d1fsz", "Font size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE))
    ), # End of column (6 space)
    column(9, h4(htmlOutput("sc1d1oupTxt")),
           uiOutput("sc1d1oup.ui"),
           downloadButton("sc1d1oup.pdf", "Download PDF"),
           downloadButton("sc1d1oup.png", "Download PNG"), br(),
           div(style="display:inline-block",
               numericInput("sc1d1oup.h", "PDF / PNG height:", width = "138px",
                            min = 4, max = 20, value = 10, step = 0.5)),
           div(style="display:inline-block",
               numericInput("sc1d1oup.w", "PDF / PNG width:", width = "138px",
                            min = 4, max = 20, value = 10, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
