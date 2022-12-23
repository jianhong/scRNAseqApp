cellInfoCellInfoUI <- function(id){
  tabPanel(
    value = id,
    HTML("CellInfo vs CellInfo"),
    h4("Cell information vs cell information on dimension reduction"),
    "In this tab, users can visualise two cell informations side-by-side ",
    "on low-dimensional representions.",
    br(),br(),
    fluidRow(
      column(3, dimensionReductionUI(id)),
      column(3, subsetCellByInfoUI(id)),
      column(6, graphicsControlUI(id))
    ),
    fluidRow(
      column(
        6, style="border-right: 2px solid black", h4("Cell information 1"),
        fluidRow(
          column(6, cellInfoUI(id, 1)),
          column(6, cellInfoPlotControlUI(id, 1))
        ),
        geneExprDotPlotUI(id, 1)
      ),
      column(
        6, h4("Cell information 2"),
        fluidRow(
          column(6, cellInfoUI(id, 2)),
          column(6, cellInfoPlotControlUI(id, 2))
        ),
        geneExprDotPlotUI(id, 2)
      )
    )
  )
}
cellInfoCellInfoServer <- function(id, dataSource, optCrt){
  moduleServer(id, function(input, output, session){
    ## input column 1
    ### Dimension Reduction
    updateDimRedSelInputPair(session, dataSource)
    ## input column 2
    updateSubsetCellUI(id, input, output, session, dataSource)

    ## plot region
    ### dropdown list
    updateCellInfoPlot(1, id, input, output, session, dataSource)
    updateCellInfoPlot(2, id, input, output, session, dataSource)
  })
}

