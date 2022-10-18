tabsubTitleUI <- function(id, title, description){
  tagList(
    htmlOutput(NS(id, paste0(title, "SubTitle"))),
    description,
    br(),br()
  )
}
graphicsControlUI <- function(id){
  tagList(
    actionButton(NS(id, "graphicTog"),
                 "Toggle graphics controls"),
    conditionalPanel(
      condition = "input.graphicTog % 2 == 1",
      ns = NS(id),
      column(
        6,
        sliderInput(NS(id, "GeneExprsiz"),
                    "Point size:",
                    min = 0, max = 4, value = 1.25, step = 0.25),
        radioButtons(NS(id, "GeneExprpsz"),
                     "Plot size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE),
        radioButtons(NS(id, "GeneExprfsz"),
                     "Font size:",
                     choices = c("Small", "Medium", "Large"),
                     selected = "Medium", inline = TRUE)
      ),
      column(
        6, radioButtons(NS(id, "GeneExprasp"), "Aspect ratio:",
                        choices = c("Square", "Fixed", "Free"),
                        selected = "Square", inline = TRUE),
        checkboxInput(NS(id, "GeneExprtxt"), "Show axis text", value = FALSE)
      )
    )
  )
}
NS0 <- function(namespace, id, postfix){
  NS(namespace, id=paste0(id, postfix))
}
geneExprPlotControlUI <- function(id, postfix=1){
  tagList(
    actionButton(NS0(id, "GeneExprtog", postfix), "Toggle plot controls"),
    conditionalPanel(
      condition = paste0("input.GeneExprtog", postfix, " % 2 == 1"), ns=NS(id),
      radioButtons(NS0(id, "GeneExprtype", postfix), "Plot type",
                   choices = c("Dotplot", "Ridgeplot"),
                   selected = "Dotplot"),
      conditionalPanel(
        condition = paste0("input.GeneExprtype", postfix, " == 'Dotplot'"),
        ns=NS(id),
        radioButtons(NS0(id, "GeneExprcol", postfix), "Colour:",
                     choices = c("White-Red", "Blue-Yellow-Red",
                                 "Yellow-Green-Purple"),
                     selected = "White-Red"),
        radioButtons(NS0(id, "GeneExprord", postfix), "Plot order:",
                     choices = c("Max-1st", "Min-1st",
                                 "Original", "Random"),
                     selected = "Max-1st", inline = TRUE),
        actionButton(NS0(id, "GeneExprrgb", postfix),
                     "Manually set max color value",
                     inline = TRUE),
        conditionalPanel(
          condition = paste0("input.GeneExprrgb", postfix, " % 2 ==1"), ns=NS(id),
          numericInput(NS0(id, "GeneExprrg", postfix), "Max value:",
                       value = 100))
      ),
      conditionalPanel(
        condition = paste0("input.GeneExprtype", postfix, " == 'Ridgeplot'"),
        ns=NS(id),
        actionButton(NS0(id, "GeneExprxlimb", postfix),
                     "Manually set x axis", inline = TRUE),
        conditionalPanel(
          condition = paste0("input.GeneExprxlimb", postfix, " % 2 ==1"),
          ns=NS(id),
          sliderInput(NS0(id, "GeneExprxlim", postfix), "Xlim range:",
                      min = -10, max = 100, value = c(-1.5, 10),
                      step = 0.1))
      )
    )
  )
}
cellInfoPlotControlUI <- function(id, postfix=1){
  tagList(
    actionButton(NS0(id, "CellInfotog", postfix), "Toggle plot controls"),
    conditionalPanel(
      condition = paste0("input.CellInfotog", postfix, " % 2 == 1"), ns=NS(id),
      radioButtons(NS0(id, "CellInfocol", postfix), "Colour (Continuous data):",
                   choices = c("White-Red", "Blue-Yellow-Red",
                               "Yellow-Green-Purple"),
                   selected = "White-Red"),
      radioButtons(NS0(id, "CellInfoord", postfix), "Plot order:",
                   choices = c("Max-1st", "Min-1st",
                               "Original", "Random"),
                   selected = "Original", inline = TRUE),
      checkboxInput(NS0(id, "CellInfolab", postfix),
                    "Show cell info labels", value = TRUE)
    )
  )
}
geneCoExprPlotControlUI <- function(id, postfix=1){
  tagList(
    actionButton(NS0(id, "CoExprtog", postfix), "Toggle plot controls"),
    conditionalPanel(
      condition = paste0("input.CoExprtog", postfix, " % 2 == 1"), ns=NS(id),
      radioButtons(NS0(id, "CoExprcol", postfix), "Colour:",
                   choices = c("Red (Gene1); Blue (Gene2)",
                               "Orange (Gene1); Blue (Gene2)",
                               "Red (Gene1); Green (Gene2)",
                               "Green (Gene1); Blue (Gene2)"),
                   selected = "Red (Gene1); Blue (Gene2)"),
      radioButtons(NS0(id, "CoExprord", postfix), "Plot order:",
                   choices = c("Max-1st", "Min-1st", "Original", "Random"),
                   selected = "Max-1st", inline = TRUE)
    )
  )
}
boxPlotControlUI <- function(id, withPoints=TRUE, withColor=FALSE){
  tagList(
    actionButton(NS(id, "plottog"), "Toggle graphics controls"),
    conditionalPanel(
      condition = "input.plottog % 2 == 1",
      ns=NS(id),
      if(withPoints) {
        sliderInput(NS(id, "plotsiz"), "Data point size:",
                    min = 0, max = 4, value = 1.25, step = 0.25)
      }else{
        span()
      },
      if(withColor){
        radioButtons(NS(id, "plotcols"), "Colour scheme:",
                     choices = c("White-Red", "Blue-Yellow-Red",
                                 "Yellow-Green-Purple"),
                     selected = "Blue-Yellow-Red")
      }else{
        span()
      },
      radioButtons(NS(id, "plotpsz"), "Plot size:",
                   choices = c("Small", "Medium", "Large"),
                   selected = "Medium", inline = TRUE),
      radioButtons(NS(id, "plotfsz"), "Font size:",
                   choices = c("Small", "Medium", "Large"),
                   selected = "Medium", inline = TRUE))
  )
}
dimensionReductionUI <- function(id){
  tagList(
    h4("Dimension Reduction"),
    fluidRow(
      column(
        12,
        selectInput(NS(id, "GeneExprdrX"),
                    "X-axis:",
                    choices = NULL),
        selectInput(NS(id, "GeneExprdrY"),
                    "Y-axis:",
                    choices = NULL))
    )
  )
}
subsetCellByInfoUI <- function(id, mini=FALSE){
  if(mini){
    tagList(
      selectInput(NS(id, "subsetCell"),
                  "Cell information to subset:",
                  choices = NULL) %>%
        helper1(cat="subsetCellInfo"),
      uiOutput(NS(id, "subsetCell.ui"))
    )
  }else{
    tagList(
      actionButton(NS(id, "subsetTogT"), "Toggle to subset cells"),
      conditionalPanel(
        condition = "input.subsetTogT % 2 == 0",
        ns = NS(id),
        selectInput(NS(id, "subsetCell"),
                    "Cell information to subset:",
                    choices = NULL) %>%
          helper1(cat="subsetCellInfo"),
        uiOutput(NS(id, "subsetCell.ui"))
      )
    )
  }
}
subsetCellByFilterUI <- function(id,
                                 label="Cell Info/Gene name to subset:",
                                 title=NULL,
                                 content=NULL){
  tagList(
    selectInput(NS(id, "filterCell"),
                label=label,
                choices = NULL) %>%
      helper1(cat="subsetCellInfo", title=title, content=content),
    uiOutput(NS(id, "filterCell.ui"))
  )
}
geneExprDotPlotUI <- function(id, postfix=1){
  tagList(
    fluidRow(column(12, uiOutput(NS0(id, "GeneExproup.ui", postfix)))),
    downloadButton(NS0(id, "GeneExproup.pdf", postfix), "Download PDF"),
    downloadButton(NS0(id, "GeneExproup.png", postfix), "Download PNG"),
    br(),
    div(style="display:inline-block",
        numericInput(NS0(id, "GeneExproup.h", postfix),
                     "PDF / PNG height:", width = "138px",
                     min = 4, max = 20, value = 6, step = 0.5)),
    div(style="display:inline-block",
        numericInput(NS0(id, "GeneExproup.w", postfix),
                     "PDF / PNG width:", width = "138px",
                     min = 4, max = 20, value = 8, step = 0.5))
  )
}

cellInfoUI <- function(id, postfix=1){
  tagList(
    selectInput(NS0(id, "CellInfo", postfix), "Cell information:",
                choices = NULL) %>%
      helper1(cat="cellInfo")
  )
}
cellInfoTblUI <- function(id, postfix=1){
  tagList(
    actionButton(NS0(id, "CellInfoTableTog", postfix),
                 "Toggle to show cell numbers / statistics"),
    conditionalPanel(
      condition = paste0("input.CellInfoTableTog", postfix, " % 2 == 1"),
      ns=NS(id),
      h4("Cell numbers / statistics"),
      radioButtons(NS0(id, "GeneExprsplt", postfix),
                   "Split continuous cell info into:",
                   choices = c("Quartile", "Decile"),
                   selected = "Decile", inline = TRUE),
      DTOutput(NS0(id, "GeneExpr.dt", postfix))
    )
  )
}

geneExprUI <- function(id, postfix=1){
  tagList(
    selectInput(NS0(id, "GeneName", postfix),
                "Gene name:", choices=NULL) %>%
      helper1(cat="geneName")
  )
}

xaxisCellInfoUI <- function(id){
  tagList(
    selectInput(NS(id, "CellInfoX"), "Cell information (X-axis):",
                choices = NULL) %>%
      helper1(cat="cellInfoX")
  )
}

yaxisCellInfoUI <- function(id){
  tagList(
    selectInput(NS(id, "CellInfoY"), "Cell Info / Gene name (Y-axis):",
                choices=NULL) %>%
      helper1(cat="cellInfoY")
  )
}

