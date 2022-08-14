tabscBubbleHeatmap <- function(){
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
      textAreaInput("scBubbleHeatmapinp", HTML("List of gene names <br />
                                         (Max 500 genes (over 50 will response slow), <br />
                                         separated by , or ; or newline):"),
                    height = "200px",
                    value = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "List of genes to plot on bubbleplot / heatmap",
               content = c("Input genes to plot",
                           "- Maximum 500 genes (due to ploting space limitations)",
                           "- Genes should be separated by comma, semicolon or newline")),
      selectInput("scBubbleHeatmapgrp", "Group by:",
                  choices = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
               title = "Cell information to group cells by",
               content = c("Select categorical cell information to group cells by",
                           "- Single cells are grouped by this categorical covariate",
                           "- Plotted as the X-axis of the bubbleplot / heatmap")),
      selectInput("scBubbleHeatmapgrp1a", "Cell information to subset:",
                  choices = NULL) %>%
        shinyhelper::helper(type = "inline", size = "m", fade = TRUE,
                            title = "Cell information to subset by:",
                            content = c("Select categorical cell information to sebset cells by",
                                        "- cells are shown in different subsets")),
      uiOutput("scBubbleHeatmapgrp1b.ui"),
      sliderInput("scBubbleHeatmapgrp1c", "Filter the cells by value",
                  min = 0, max = 10, value = 0),
      radioButtons("scBubbleHeatmapplt", "Plot type:",
                   choices = c("Bubbleplot", "Heatmap"),
                   selected = "Bubbleplot", inline = TRUE),
      checkboxInput("scBubbleHeatmapscl", "Scale gene expression", value = TRUE),
      checkboxInput("scBubbleHeatmaprow", "Cluster rows (genes)", value = TRUE),
      checkboxInput("scBubbleHeatmapcol", "Cluster columns (samples)", value = FALSE),
      br(),
      actionButton("scBubbleHeatmaptog", "Toggle graphics controls"),
      conditionalPanel(
        condition = "input.scBubbleHeatmaptog % 2 == 1",
        radioButtons("scBubbleHeatmapcols", "Colour scheme:",
                     choices = c("White-Red", "Blue-Yellow-Red",
                                 "Yellow-Green-Purple"),
                     selected = "Blue-Yellow-Red"),
        radioButtons("scBubbleHeatmappsz", "Plot size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE),
        radioButtons("scBubbleHeatmapfsz", "Font size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE))
    ), # End of column (6 space)
    column(9, h4(htmlOutput("scBubbleHeatmapoupTxt")),
           uiOutput("scBubbleHeatmapoup.ui"),
           downloadButton("scBubbleHeatmapoup.pdf", "Download PDF"),
           downloadButton("scBubbleHeatmapoup.png", "Download PNG"), br(),
           div(style="display:inline-block",
               numericInput("scBubbleHeatmapoup.h", "PDF / PNG height:", width = "138px",
                            min = 4, max = 20, value = 10, step = 0.5)),
           div(style="display:inline-block",
               numericInput("scBubbleHeatmapoup.w", "PDF / PNG width:", width = "138px",
                            min = 4, max = 20, value = 10, step = 0.5))
    )  # End of column (6 space)
  )    # End of fluidRow (4 space)
)
}
